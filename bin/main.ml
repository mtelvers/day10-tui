open Notty
open Day10_tui_lib

(* open Notty.Infix *)
module NottyTerm = Notty_unix.Term

let column_width = 8

module StringSet = Set.Make(String)

module String = struct
  include String
  let strstr haystack needle =
    let haystack_len = String.length haystack in
    let needle_len = String.length needle in

    if needle_len = 0 then Some 0
    else if needle_len > haystack_len then None
    else
      let rec search pos =
        if pos > haystack_len - needle_len then None
        else
          let rec match_at i =
            if i = needle_len then true
            else if haystack.[pos + i] = needle.[i] then match_at (i + 1)
            else false
          in
          if match_at 0 then Some pos
          else search (pos + 1)
      in
      search 0
end

type availability_status =
  | Downloaded        (* filled circle - file exists locally *)
  | Available         (* empty circle - available on server *)
  | Not_available     (* dash - not on server *)

type commit_info = {
  sha : string;
  message : string;
  date : string;
  availability : availability_status;
}

type build_key = {
  package : string;
  compiler : string;
}

type build_result = {
  status : string;
  log : string option;
  solution : string option;
}

type detail_info = {
  key : build_key;
  result : build_result;
  log_lines : string list;
  solution_lines : string list;
  detail_scroll : int;
}

type home_info = {
  commits : commit_info list;
  selected_commit : int;
  scroll_offset : int;
  selected_commits : int list; (* List of selected commit indices for comparison *)
}

type package_diff =
  | New_package of string * (string * string) list  (* package_name, (compiler, status) list *)
  | Removed_package of string * (string * string) list
  | Status_changed of string * (string * string * string) list (* package_name, (compiler, old_status, new_status) list *)

type diff_info = {
  selected_commits : commit_info list;
  new_packages : package_diff list;
  removed_packages : package_diff list;
  status_changes : package_diff list;
  diff_scroll : int;
}

type app_mode =
  | Home_view of home_info
  | Table_view
  | Detail_view of detail_info
  | Diff_view of diff_info

type table_filter =
  | No_filter
  | Filter_failed
  | Filter_no_solution
  | Filter_dependency_failed
  | Filter_success

type column_indices = {
  name_idx : int;
  status_idx : int;
  compiler_idx : int;
  log_idx : int;
  solution_idx : int;
}

type tui_state = {
  opam_repo_path : string;
  num_commits : int; (* Number of commits to show *)
  current_filename : string; (* Current parquet file being viewed *)
  build_map : (build_key, build_result) Hashtbl.t;
  full_data_loaded : bool; (* Whether the full log/solution data has been loaded *)
  packages : string array;
  compilers : string array;
  column_indices : column_indices option; (* Cached column indices to avoid re-reading schema *)
  scroll_x : int; (* horizontal scroll for compilers *)
  scroll_y : int; (* vertical scroll for packages *)
  selected_x : int; (* selected compiler column *)
  selected_y : int; (* selected package row *)
  table_filter : table_filter; (* Current filter for table view *)
  search_mode : bool; (* Whether we're in search input mode *)
  search_text : string; (* Current search text *)
  mode : app_mode;
}

(* Find the remote and default branch for ocaml/oxcaml opam-repository *)
let find_repository_remote_and_branch opam_repo_path =
  let remote_cmd = Filename.quote_command "git" [ "-C"; opam_repo_path; "remote"; "-v" ] in
  let remote =
    try
      let ic = Unix.open_process_in remote_cmd in
      let rec find_official_remote () =
        try
          let line = input_line ic in
          if String.strstr line "https://github.com/ocaml/opam-repository" <> None ||
             String.strstr line "https://github.com/oxcaml/opam-repository" <> None then (
            (* Extract remote name (first word before whitespace) *)
            match String.split_on_char '\t' line with
            | remote_name :: _ -> Some remote_name
            | [] ->
                match String.split_on_char ' ' line with
                | remote_name :: _ -> Some remote_name
                | [] -> None
          ) else
            find_official_remote ()
        with End_of_file -> None
      in
      let result = find_official_remote () in
      let _ = Unix.close_process_in ic in
      match result with
      | Some r -> r
      | None -> "origin" (* fallback *)
    with _ -> "origin"
  in
  (* Determine default branch (master or main) *)
  let branch =
    let branch_cmd = Filename.quote_command "git" [ "-C"; opam_repo_path; "ls-remote"; "--symref"; remote; "HEAD" ] ~stderr:"/dev/null" in
    try
      let ic = Unix.open_process_in branch_cmd in
      let line = input_line ic in
      let _ = Unix.close_process_in ic in
      (* Parse "ref: refs/heads/main	HEAD" to extract "main" *)
      if String.strstr line "ref: refs/heads/" <> None then
        let parts = String.split_on_char '/' line in
        match List.rev parts with
        | head_part :: _ ->
            let branch_part = String.split_on_char '\t' head_part in
            (match branch_part with
            | branch :: _ -> branch
            | [] -> "master")
        | [] -> "master"
      else
        "master"
    with _ -> "master" (* fallback *)
  in
  (remote, branch)

(* Determine data URL based on repository remote *)
let get_data_url opam_repo_path =
  let remote_cmd = Filename.quote_command "git" [ "-C"; opam_repo_path; "remote"; "-v" ] in
  try
    let ic = Unix.open_process_in remote_cmd in
    let rec find_repo_type () =
      try
        let line = input_line ic in
        if String.strstr line "https://github.com/ocaml/opam-repository" <> None then
          Some "https://www.cl.cam.ac.uk/~mte24/day10/"
        else if String.strstr line "https://github.com/oxcaml/opam-repository" <> None then
          Some "https://www.cl.cam.ac.uk/~mte24/day10-ox/"
        else
          find_repo_type ()
      with End_of_file -> None
    in
    let result = find_repo_type () in
    let _ = Unix.close_process_in ic in
    result
  with _ -> None

(* Fetch and parse server directory listing to get available files *)
let get_available_files opam_repo_path =
  match get_data_url opam_repo_path with
  | None -> []
  | Some url ->
  let cmd = Filename.quote_command "curl" [ "-s"; "-f"; url ] ~stderr:"/dev/null" in
  try
    let ic = Unix.open_process_in cmd in
    let available_files = ref [] in
    let rec read_lines () =
      try
        let line = input_line ic in
        (* Look for lines containing .parquet files in href attributes *)
        (match String.strstr line ".parquet" with
        | Some parquet_idx ->
            (match String.rindex_from_opt line (parquet_idx - 1) '"' with
            | Some quote_idx ->
                let start_idx = quote_idx + 1 in
                let end_idx = parquet_idx + 8 in (* ".parquet" is 8 chars *)
                let filename = String.sub line start_idx (end_idx - start_idx) in
                available_files := filename :: !available_files
            | None -> () (* No quote found *))
        | None -> ());
        read_lines ()
      with End_of_file -> ()
    in
    read_lines ();
    let _ = Unix.close_process_in ic in
    !available_files
  with _ -> []

let fetch_repository opam_repo_path =
  Printf.printf "Fetching latest changes from remote repository...\n";
  let (remote, branch) = find_repository_remote_and_branch opam_repo_path in

  Printf.printf "Using remote: %s\n" remote;

  let fetch_cmd = Filename.quote_command "git" [ "-C"; opam_repo_path; "fetch"; remote ] in
  let fetch_exit = Sys.command fetch_cmd in
  if fetch_exit = 0 then (
    Printf.printf "Resetting to latest %s...\n" branch;
    let reset_cmd = Filename.quote_command "git" [ "-C"; opam_repo_path; "reset"; "--hard"; remote ^ "/" ^ branch ] in
    let reset_exit = Sys.command reset_cmd in
    if reset_exit = 0 then
      Printf.printf "Repository updated successfully.\n"
    else
      Printf.printf "Warning: Failed to reset to %s/%s\n" remote branch
  ) else
    Printf.printf "Warning: Failed to fetch from %s\n" remote

let get_git_commits opam_repo_path num_commits =
  (* Always show commits from the tip of the remote branch *)
  let (remote, branch) = find_repository_remote_and_branch opam_repo_path in
  let branch_ref = remote ^ "/" ^ branch in
  let cmd = Filename.quote_command "git" [ "-C"; opam_repo_path; "log"; "--oneline"; "--merges"; "-n"; string_of_int num_commits; "--format=%H|%s|%ci"; branch_ref ] ~stderr:"/dev/null" in
  try
    (* Get the list of available files from server once *)
    let available_files = get_available_files opam_repo_path in
    let ic = Unix.open_process_in cmd in
    let result =
      try
        let rec read_lines acc =
          try
            let line = input_line ic in
            let parts = String.split_on_char '|' line in
            match parts with
            | [ sha; message; date ] ->
                let filename = sha ^ ".parquet" in
                let availability =
                  if Sys.file_exists filename then
                    Downloaded
                  else if List.mem filename available_files then
                    Available
                  else
                    Not_available
                in
                let commit = { sha; message; date; availability } in
                read_lines (commit :: acc)
            | _ -> read_lines acc
          with
          | End_of_file -> acc
        in
        let commits = read_lines [] in
        let exit_status = Unix.close_process_in ic in
        match exit_status with
        | Unix.WEXITED 0 -> `Success (List.rev commits)
        | _ -> `Error (Printf.sprintf "Git command failed for %s" opam_repo_path)
      with
      | exn ->
          let _ = Unix.close_process_in ic in
          raise exn
    in
    match result with
    | `Success commits -> commits
    | `Error msg ->
        Printf.printf "Warning: %s\n" msg;
        []
  with
  | Unix.Unix_error (error, _, _) ->
      Printf.printf "Error accessing git repository: %s\n" (Unix.error_message error);
      []
  | exn ->
      Printf.printf "Unexpected error getting git commits: %s\n" (Printexc.to_string exn);
      []

let download_parquet opam_repo_path sha =
  let filename = sha ^ ".parquet" in
  if not (Sys.file_exists filename) then
    match get_data_url opam_repo_path with
    | None -> `No_data_source
    | Some base_url ->
    let url = Printf.sprintf "%s%s.parquet" base_url sha in
    let cmd = Filename.quote_command "curl" [ "-s"; "-f"; "-o"; filename; url ] ~stderr:"/dev/null" in
    let exit_code = Sys.command cmd in
    if exit_code = 0 && Sys.file_exists filename then `Success
    else (
      (* Clean up partial download *)
      if Sys.file_exists filename then Sys.remove filename;
      let error_msg =
        match exit_code with
        | 22 -> "File not found on server"
        | 6 -> "Could not resolve host"
        | 7 -> "Failed to connect to server"
        | 28 -> "Connection timeout"
        | _ -> Printf.sprintf "Download failed (exit code %d)" exit_code
      in
      `Error error_msg)
  else `Success

let analyze_data filename =
  (* Get schema to find column indices *)
  let schema = Arrow.Parquet_reader.schema filename in

  (* Extract field names from schema children (parquet tables have columns as children) *)
  let field_names = List.map (fun child -> child.Arrow.Wrapper.Schema.name) schema.Arrow.Wrapper.Schema.children in

  (* Find the indices of the columns we need *)
  let find_column_index name =
    match List.find_index (String.equal name) field_names with
    | Some idx -> idx
    | None -> failwith ("cannot find column " ^ name)
  in

  let name_idx = find_column_index "name" in
  let status_idx = find_column_index "status" in
  let compiler_idx = find_column_index "compiler" in
  let log_idx = find_column_index "log" in
  let solution_idx = find_column_index "solution" in

  (* Create column indices record *)
  let column_indices = { name_idx; status_idx; compiler_idx; log_idx; solution_idx } in

  (* Load only the columns we need for table view *)
  let table = Arrow.Parquet_reader.table filename ~column_idxs:[name_idx; status_idx; compiler_idx] in
  let name_col = Arrow.Wrapper.Column.read_utf8 table ~column:(`Name "name") in
  let status_col = Arrow.Wrapper.Column.read_utf8_opt table ~column:(`Name "status") in
  let compiler_col = Arrow.Wrapper.Column.read_utf8 table ~column:(`Name "compiler") in

  (* Extract unique packages and compilers *)
  let unique_packages = Array.of_list (List.sort_uniq String.compare (Array.to_list name_col)) in
  let unique_compilers = Array.of_list (List.sort_uniq String.compare (Array.to_list compiler_col)) in

  (* Create a lookup map: (package, compiler) -> status only (no log/solution yet) *)
  let build_map = Hashtbl.create 1000 in
  Array.iteri (fun i package ->
      let compiler = compiler_col.(i) in
      match status_col.(i) with
      | Some status ->
          let key = { package; compiler } in
          let result = { status; log = None; solution = None } in
          Hashtbl.replace build_map key result
      | _ -> ()) name_col;

  (build_map, unique_packages, unique_compilers, column_indices)

(* Load full data including log and solution columns and update the hashtable *)
(* Compare two build maps and generate diff information *)
let compare_build_maps old_map old_packages old_compilers new_map new_packages new_compilers =
  let old_package_set = Array.to_list old_packages |> List.fold_left (fun acc p -> StringSet.add p acc) StringSet.empty in
  let new_package_set = Array.to_list new_packages |> List.fold_left (fun acc p -> StringSet.add p acc) StringSet.empty in

  (* Find new and removed packages *)
  let new_packages_list = StringSet.diff new_package_set old_package_set |> StringSet.elements in
  let removed_packages_list = StringSet.diff old_package_set new_package_set |> StringSet.elements in

  (* Find packages with status changes *)
  let common_packages = StringSet.inter old_package_set new_package_set |> StringSet.elements in
  let changed_packages = List.filter_map (fun package ->
    let old_statuses = Array.to_list old_compilers |> List.filter_map (fun compiler ->
      match Hashtbl.find_opt old_map { package; compiler } with
      | Some result -> Some (compiler, result.status)
      | None -> None
    ) in
    let new_statuses = Array.to_list new_compilers |> List.filter_map (fun compiler ->
      match Hashtbl.find_opt new_map { package; compiler } with
      | Some result -> Some (compiler, result.status)
      | None -> None
    ) in

    let changes = List.filter_map (fun (compiler, new_status) ->
      match List.assoc_opt compiler old_statuses with
      | Some old_status when old_status <> new_status -> Some (compiler, old_status, new_status)
      | _ -> None
    ) new_statuses in

    if List.length changes > 0 then
      Some (Status_changed (package, changes))
    else
      None
  ) common_packages in

  (* Build new/removed package info with their statuses *)
  let new_packages_with_status = List.map (fun package ->
    let statuses = Array.to_list new_compilers |> List.filter_map (fun compiler ->
      match Hashtbl.find_opt new_map { package; compiler } with
      | Some result -> Some (compiler, result.status)
      | None -> None
    ) in
    New_package (package, statuses)
  ) new_packages_list in

  let removed_packages_with_status = List.map (fun package ->
    let statuses = Array.to_list old_compilers |> List.filter_map (fun compiler ->
      match Hashtbl.find_opt old_map { package; compiler } with
      | Some result -> Some (compiler, result.status)
      | None -> None
    ) in
    Removed_package (package, statuses)
  ) removed_packages_list in

  (new_packages_with_status, removed_packages_with_status, changed_packages)

let load_full_data filename build_map column_indices =
  (* Use cached column indices instead of re-reading schema *)
  let { name_idx; compiler_idx; log_idx; solution_idx; _ } = column_indices in

  (* Load only the columns we need *)
  let table = Arrow.Parquet_reader.table filename ~column_idxs:[name_idx; compiler_idx; log_idx; solution_idx] in
  let name_col = Arrow.Wrapper.Column.read_utf8 table ~column:(`Name "name") in
  let compiler_col = Arrow.Wrapper.Column.read_utf8 table ~column:(`Name "compiler") in
  let log_col = Arrow.Wrapper.Column.read_utf8_opt table ~column:(`Name "log") in
  let solution_col = Arrow.Wrapper.Column.read_utf8_opt table ~column:(`Name "solution") in

  (* Update the existing hashtable with log and solution data *)
  Array.iteri (fun i package ->
      let compiler = compiler_col.(i) in
      let key = { package; compiler } in
      match Hashtbl.find_opt build_map key with
      | Some existing_result ->
          let updated_result = { existing_result with log = log_col.(i); solution = solution_col.(i) } in
          Hashtbl.replace build_map key updated_result
      | None -> () (* Skip entries not in the original map *)
  ) name_col


let status_color = function
  | "success" -> A.fg A.green
  | "failure" -> A.fg A.red
  | "dependency_failed" -> A.fg A.yellow
  | "no_solution" -> A.fg A.magenta
  | _ -> A.fg A.white

let status_char = function
  | "success" -> "✓"
  | "failure" -> "✗"
  | "dependency_failed" -> "!"
  | "no_solution" -> "○"
  | _ -> "?"

let draw_home_view { commits; selected_commit; scroll_offset; selected_commits } (w, h) =
  let list_items =
    List.mapi (fun idx commit ->
        let download_indicator =
          match commit.availability with
          | Downloaded -> Some "●"      (* filled circle *)
          | Available -> Some "○"       (* empty circle *)
          | Not_available -> Some "-"   (* dash *)
        in
        let selection_indicator = if List.mem idx selected_commits then "[*]" else "[ ]" in
        let short_sha = String.sub commit.sha 0 (min 8 (String.length commit.sha)) in
        let message_truncated = if String.length commit.message > 60 then String.sub commit.message 0 60 else commit.message in
        let display_text = Printf.sprintf "%s %s %s - %s" selection_indicator short_sha commit.date message_truncated in
        Day10_tui_lib.List_widget.List.{ content = commit; display_text; status_indicator = download_indicator; attr = A.(fg white) }) commits
  in

  let config =
    Day10_tui_lib.List_widget.List.
      {
        items = list_items;
        selected_item = selected_commit;
        scroll_offset;
        title = "Select a commit to analyze:";
        help_text = "↑/↓: navigate, PgUp/PgDn: page, Home/End: first/last, Enter: select/download, q: quit";
      }
  in

  Day10_tui_lib.List_widget.List.draw_list config (w, h)

let show_error_message term error_msg =
  let config = Day10_tui_lib.Dialog_widget.Dialog.{ dialog_type = Error; title = ""; message = error_msg; help_text = "Press any key to continue..." } in
  Day10_tui_lib.Dialog_widget.Dialog.show_dialog term config

let draw_diff_view diff_info (_w, h) =
  let content = [
    "=== COMMIT COMPARISON ===";
    "";
    Printf.sprintf "Comparing %d commits:" (List.length diff_info.selected_commits);
  ] @ (List.map (fun commit -> Printf.sprintf "  %s - %s"
    (String.sub commit.sha 0 8) commit.message) diff_info.selected_commits) @ [
    "";
    "📈 NEW PACKAGES:";
  ] @ (List.map (function
    | New_package (pkg, _) -> Printf.sprintf "  + %s" pkg
    | _ -> "") diff_info.new_packages) @ [
    "";
    "🗑️ REMOVED PACKAGES:";
  ] @ (List.map (function
    | Removed_package (pkg, _) -> Printf.sprintf "  - %s" pkg
    | _ -> "") diff_info.removed_packages) @ [
    "";
    "📊 STATUS CHANGES:";
  ] @ (List.fold_left (fun acc -> function
    | Status_changed (pkg, changes) ->
        let pkg_line = Printf.sprintf "  ~ %s:" pkg in
        let change_lines = List.map (fun (compiler, old_status, new_status) ->
          Printf.sprintf "    %s: %s → %s" compiler old_status new_status
        ) changes in
        acc @ [pkg_line] @ change_lines
    | _ -> acc) [] diff_info.status_changes) @ [
    "";
    "Press Q/Escape to return to commit list";
  ] in

  let visible_content =
    let start_line = diff_info.diff_scroll in
    let end_line = min (List.length content) (start_line + h - 1) in
    if start_line < List.length content then
      let rec take n lst = match n, lst with
        | 0, _ -> []
        | _n, [] -> []
        | n, x :: xs -> x :: take (n - 1) xs
      in
      let rec drop n lst = match n, lst with
        | 0, lst -> lst
        | _n, [] -> []
        | n, _ :: xs -> drop (n - 1) xs
      in
      content |> drop start_line |> take (end_line - start_line)
    else
      []
  in

  let lines = List.map (fun line -> I.string A.(fg white) line) visible_content in
  match lines with
  | [] -> I.string A.(fg white) "No diff data"
  | hd :: tl -> List.fold_left I.(<->) hd tl

let draw_detail_view detail (w, h) =
  let header = Printf.sprintf "Package: %s | Compiler: %s | Status: %s" detail.key.package detail.key.compiler detail.result.status in

  let build_log_section =
    Day10_tui_lib.Text_viewer_widget.TextViewer.
      {
        title = "Build Log:";
        title_attr = A.(fg cyan ++ st bold);
        lines = (if List.is_empty detail.log_lines then [ "No build log available" ] else detail.log_lines);
        line_attr = (if List.is_empty detail.log_lines then A.(fg yellow) else A.(fg white));
      }
  in

  let solution_section =
    Day10_tui_lib.Text_viewer_widget.TextViewer.
      {
        title = "Solution:";
        title_attr = A.(fg green ++ st bold);
        lines = (if List.is_empty detail.solution_lines then [ "No solution available" ] else detail.solution_lines);
        line_attr = (if List.is_empty detail.solution_lines then A.(fg yellow) else A.(fg white));
      }
  in

  let config =
    Day10_tui_lib.Text_viewer_widget.TextViewer.
      {
        header;
        header_attr = A.(fg white ++ st bold);
        sections = [ build_log_section; solution_section ];
        scroll_offset = detail.detail_scroll;
        help_text = "↑/↓: scroll, PgUp/PgDn: page, Home/End: top/bottom, Esc/q: back to table";
      }
  in

  Day10_tui_lib.Text_viewer_widget.TextViewer.draw_viewer config (w, h)

(* Filter packages based on current filter and selected compiler *)
let filter_packages packages compilers build_map selected_compiler filter_type search_text =
  let selected_compiler_name = compilers.(selected_compiler) in
  let matches_search package =
    if String.length search_text = 0 then true
    else
      let package_lower = String.lowercase_ascii package in
      let search_lower = String.lowercase_ascii search_text in
      match String.strstr package_lower search_lower with
      | Some _ -> true
      | None -> false
  in
  Array.to_list packages |> List.filter (fun package ->
    matches_search package &&
    match filter_type with
    | No_filter -> true
    | Filter_failed ->
        (match Hashtbl.find_opt build_map { package; compiler = selected_compiler_name } with
        | Some result -> result.status = "failure"
        | None -> false)
    | Filter_no_solution ->
        (match Hashtbl.find_opt build_map { package; compiler = selected_compiler_name } with
        | Some result -> result.status = "no_solution"
        | None -> false)
    | Filter_dependency_failed ->
        (match Hashtbl.find_opt build_map { package; compiler = selected_compiler_name } with
        | Some result -> result.status = "dependency_failed"
        | None -> false)
    | Filter_success ->
        (match Hashtbl.find_opt build_map { package; compiler = selected_compiler_name } with
        | Some result -> result.status = "success"
        | None -> false)
  ) |> Array.of_list

let draw_table state (w, h) =
  (* Filter packages based on current filter and selected compiler *)
  let filtered_packages = filter_packages state.packages state.compilers state.build_map state.selected_x state.table_filter state.search_text in

  let get_cell ~row ~column =
    if String.equal column "_row_name" then { Table_widget.Table.text = row; attr = A.(fg white) }
    else
      let key = { package = row; compiler = column } in
      match Hashtbl.find_opt state.build_map key with
      | Some result ->
          let color = status_color result.status in
          let char = status_char result.status in
          { Table_widget.Table.text = char; attr = color }
      | None -> { Table_widget.Table.text = "-"; attr = A.(fg white) }
  in

  let filter_status_text = match state.table_filter with
    | No_filter -> ""
    | Filter_failed -> " [FAILED]"
    | Filter_no_solution -> " [NO_SOLUTION]"
    | Filter_dependency_failed -> " [DEP_FAILED]"
    | Filter_success -> " [SUCCESS]"
  in

  let total_packages = Array.length state.packages in
  let filtered_count = Array.length filtered_packages in
  let count_text = if filtered_count = total_packages then
    Printf.sprintf " (%d packages)" total_packages
  else
    Printf.sprintf " (%d/%d packages)" filtered_count total_packages
  in
  let search_text = if String.length state.search_text > 0 then
    Printf.sprintf " search: \"%s\"" state.search_text
  else
    ""
  in
  let search_mode_text = if state.search_mode then " [SEARCHING]" else "" in
  let help_text = Printf.sprintf "Arrows: navigate | Enter: details | Q: back | /: search | f/n/d/s: filter%s%s%s%s" filter_status_text count_text search_text search_mode_text in

  let config =
    {
      Table_widget.Table.rows = filtered_packages;
      columns = state.compilers;
      get_cell;
      row_height = 1;
      column_width;
      selected_row = Some state.selected_y;
      selected_col = Some state.selected_x;
      scroll_row = state.scroll_y;
      scroll_col = state.scroll_x;
      help_text;
    }
  in

  Table_widget.Table.draw_table config (w, h)

let sanitize_text text =
  String.map (fun c ->
      let code = Char.code c in
      if code < 32 && code <> 10 then ' ' else c) text

(* Helper function to toggle selection of a commit *)
let toggle_commit_selection selected_commits idx =
  if List.mem idx selected_commits then
    List.filter ((<>) idx) selected_commits
  else
    idx :: selected_commits

let handle_home_event term state home event =
  match event with
  | `Key (`Arrow `Up, []) ->
      let new_selected = max 0 (home.selected_commit - 1) in
      let new_scroll = if new_selected < home.scroll_offset then new_selected else home.scroll_offset in
      let new_home = { home with selected_commit = new_selected; scroll_offset = new_scroll } in
      `Continue { state with mode = Home_view new_home }
  | `Key (`Arrow `Down, []) ->
      let max_idx = List.length home.commits - 1 in
      let new_selected = min max_idx (home.selected_commit + 1) in
      let content_height = snd (NottyTerm.size term) - 3 in
      let new_scroll = if new_selected >= home.scroll_offset + content_height then new_selected - content_height + 1 else home.scroll_offset in
      let new_home = { home with selected_commit = new_selected; scroll_offset = new_scroll } in
      `Continue { state with mode = Home_view new_home }
  | `Key (`Page `Up, []) ->
      let content_height = snd (NottyTerm.size term) - 3 in
      let page_size = max 1 (content_height - 1) in
      let new_selected = max 0 (home.selected_commit - page_size) in
      let new_scroll = max 0 (min new_selected home.scroll_offset) in
      let new_home = { home with selected_commit = new_selected; scroll_offset = new_scroll } in
      `Continue { state with mode = Home_view new_home }
  | `Key (`Page `Down, []) ->
      let max_idx = List.length home.commits - 1 in
      let content_height = snd (NottyTerm.size term) - 3 in
      let page_size = max 1 (content_height - 1) in
      let new_selected = min max_idx (home.selected_commit + page_size) in
      let new_scroll = if new_selected >= home.scroll_offset + content_height then new_selected - content_height + 1 else home.scroll_offset in
      let new_home = { home with selected_commit = new_selected; scroll_offset = new_scroll } in
      `Continue { state with mode = Home_view new_home }
  | `Key (`Home, []) ->
      let new_home = { home with selected_commit = 0; scroll_offset = 0 } in
      `Continue { state with mode = Home_view new_home }
  | `Key (`End, []) ->
      let max_idx = List.length home.commits - 1 in
      let content_height = snd (NottyTerm.size term) - 3 in
      let new_scroll = max 0 (max_idx - content_height + 1) in
      let new_home = { home with selected_commit = max_idx; scroll_offset = new_scroll } in
      `Continue { state with mode = Home_view new_home }
  | `Key (`ASCII ' ', []) ->
      (* Toggle selection on current commit and move down *)
      let new_selected_commits = toggle_commit_selection home.selected_commits home.selected_commit in
      let max_idx = List.length home.commits - 1 in
      let new_selected = min max_idx (home.selected_commit + 1) in
      let content_height = snd (NottyTerm.size term) - 3 in
      let new_scroll = if new_selected >= home.scroll_offset + content_height then new_selected - content_height + 1 else home.scroll_offset in
      let new_home = { home with selected_commit = new_selected; scroll_offset = new_scroll; selected_commits = new_selected_commits } in
      `Continue { state with mode = Home_view new_home }
  | `Key (`Enter, []) -> (
      if List.length home.selected_commits >= 2 then (
        (* Multi-commit diff mode *)
        try
          let selected_commit_objs = List.filter_map (fun idx ->
            List.nth_opt home.commits idx
          ) home.selected_commits in

          (* Load data for all selected commits *)
          let commit_data_list = List.filter_map (fun commit ->
            let filename = commit.sha ^ ".parquet" in
            match download_parquet state.opam_repo_path commit.sha with
            | `Success -> (
                try
                  let build_map, packages, compilers, _column_indices = analyze_data filename in
                  Some (commit, build_map, packages, compilers)
                with _ -> None
            )
            | `No_data_source -> None
            | `Error _ -> None
          ) selected_commit_objs in

          if List.length commit_data_list >= 2 then (
            (* Compare first and last commits *)
            let (_, old_map, old_packages, old_compilers) = List.hd commit_data_list in
            let (_, new_map, new_packages, new_compilers) = List.hd (List.rev commit_data_list) in

            let (new_packages_diff, removed_packages_diff, status_changes_diff) =
              compare_build_maps old_map old_packages old_compilers new_map new_packages new_compilers in

            let diff_info = {
              selected_commits = selected_commit_objs;
              new_packages = new_packages_diff;
              removed_packages = removed_packages_diff;
              status_changes = status_changes_diff;
              diff_scroll = 0;
            } in
            `Continue { state with mode = Diff_view diff_info }
          ) else (
            show_error_message term "Could not load data for selected commits";
            `Continue state
          )
        with exn ->
          let error_msg = Printf.sprintf "Error comparing commits: %s" (Printexc.to_string exn) in
          show_error_message term error_msg;
          `Continue state
      ) else (
        (* Single commit mode - normal behavior *)
        match List.nth_opt home.commits home.selected_commit with
        | Some commit -> (
            let filename = commit.sha ^ ".parquet" in
            match download_parquet state.opam_repo_path commit.sha with
            | `Success -> (
                try
                  let build_map, packages, compilers, column_indices = analyze_data filename in
                  let new_state = { state with current_filename = filename; build_map; packages; compilers; column_indices = Some column_indices; full_data_loaded = false; table_filter = No_filter; search_mode = false; search_text = ""; mode = Table_view } in
                  `Continue new_state
                with
                | exn ->
                    let raw_error = Printexc.to_string exn in
                    let clean_error = sanitize_text raw_error in
                    let error_msg = Printf.sprintf "Failed to parse parquet file: %s" clean_error in
                    show_error_message term error_msg;
                    `Continue state)
            | `No_data_source ->
                show_error_message term "No data source available for this repository type";
                `Continue state
            | `Error error_msg ->
                show_error_message term error_msg;
                `Continue state)
        | None -> `Continue state
      ))
  | `Key (`ASCII 'q', [])
  | `Key (`Escape, []) ->
      `Quit
  | _ -> `Continue state

let handle_detail_event term state detail event =
  match event with
  | `Key (`Arrow `Up, []) ->
      let new_scroll = max 0 (detail.detail_scroll - 1) in
      let new_detail = { detail with detail_scroll = new_scroll } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`Arrow `Down, []) ->
      let total_lines = List.length detail.log_lines + List.length detail.solution_lines + 4 in
      let max_scroll = max 0 (total_lines - (snd (NottyTerm.size term) - 3)) in
      let new_scroll = min max_scroll (detail.detail_scroll + 1) in
      let new_detail = { detail with detail_scroll = new_scroll } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`Page `Up, []) ->
      let content_height = snd (NottyTerm.size term) - 3 in
      let page_size = max 1 (content_height - 1) in
      let new_scroll = max 0 (detail.detail_scroll - page_size) in
      let new_detail = { detail with detail_scroll = new_scroll } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`Page `Down, []) ->
      let total_lines = List.length detail.log_lines + List.length detail.solution_lines + 4 in
      let max_scroll = max 0 (total_lines - (snd (NottyTerm.size term) - 3)) in
      let content_height = snd (NottyTerm.size term) - 3 in
      let page_size = max 1 (content_height - 1) in
      let new_scroll = min max_scroll (detail.detail_scroll + page_size) in
      let new_detail = { detail with detail_scroll = new_scroll } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`Home, []) ->
      let new_detail = { detail with detail_scroll = 0 } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`End, []) ->
      let total_lines = List.length detail.log_lines + List.length detail.solution_lines + 4 in
      let max_scroll = max 0 (total_lines - (snd (NottyTerm.size term) - 3)) in
      let new_detail = { detail with detail_scroll = max_scroll } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`Escape, [])
  | `Key (`ASCII 'q', []) ->
      `Continue { state with mode = Table_view }
  | _ -> `Continue state

let handle_diff_event _term state diff_info event =
  match event with
  | `Key (`Arrow `Up, []) ->
      let new_scroll = max 0 (diff_info.diff_scroll - 1) in
      let new_diff = { diff_info with diff_scroll = new_scroll } in
      `Continue { state with mode = Diff_view new_diff }
  | `Key (`Arrow `Down, []) ->
      let new_scroll = diff_info.diff_scroll + 1 in
      let new_diff = { diff_info with diff_scroll = new_scroll } in
      `Continue { state with mode = Diff_view new_diff }
  | `Key (`ASCII 'q', [])
  | `Key (`Escape, []) ->
      let commits = get_git_commits state.opam_repo_path state.num_commits in
      let home = { commits; selected_commit = 0; scroll_offset = 0; selected_commits = [] } in
      `Continue { state with mode = Home_view home }
  | _ -> `Continue state

let handle_table_event term state event =
  if state.search_mode then
    match event with
    | `Key (`Escape, []) ->
        `Continue { state with search_mode = false; search_text = ""; selected_y = 0; scroll_y = 0 }
    | `Key (`Enter, []) ->
        `Continue { state with search_mode = false; selected_y = 0; scroll_y = 0 }
    | `Key (`Backspace, []) ->
        let new_text = if String.length state.search_text > 0 then
          String.sub state.search_text 0 (String.length state.search_text - 1)
        else
          state.search_text
        in
        `Continue { state with search_text = new_text; selected_y = 0; scroll_y = 0 }
    | `Key (`ASCII c, []) when c >= ' ' && c <= '~' ->
        let new_text = state.search_text ^ (String.make 1 c) in
        `Continue { state with search_text = new_text; selected_y = 0; scroll_y = 0 }
    | _ -> `Continue state
  else
    match event with
  | `Key (`Arrow `Up, []) ->
      let new_y = max 0 (state.selected_y - 1) in
      let new_scroll_y = if new_y < state.scroll_y then new_y else state.scroll_y in
      `Continue { state with selected_y = new_y; scroll_y = new_scroll_y }
  | `Key (`Arrow `Down, []) ->
      let filtered_packages = filter_packages state.packages state.compilers state.build_map state.selected_x state.table_filter state.search_text in
      let max_y = Array.length filtered_packages - 1 in
      let new_y = min max_y (state.selected_y + 1) in
      let term_height = snd (NottyTerm.size term) - 4 in
      let new_scroll_y = if new_y >= state.scroll_y + term_height then new_y - term_height + 1 else state.scroll_y in
      `Continue { state with selected_y = new_y; scroll_y = new_scroll_y }
  | `Key (`Arrow `Left, []) ->
      let new_x = max 0 (state.selected_x - 1) in
      let new_scroll_x = if new_x < state.scroll_x then new_x else state.scroll_x in
      `Continue { state with selected_x = new_x; scroll_x = new_scroll_x }
  | `Key (`Arrow `Right, []) ->
      let max_x = Array.length state.compilers - 1 in
      let new_x = min max_x (state.selected_x + 1) in
      let term_width = fst (NottyTerm.size term) in
      let visible_compilers = (term_width - 30) / column_width in
      let new_scroll_x = if new_x >= state.scroll_x + visible_compilers then new_x - visible_compilers + 1 else state.scroll_x in
      `Continue { state with selected_x = new_x; scroll_x = new_scroll_x }
  | `Key (`Page `Up, []) ->
      let term_height = snd (NottyTerm.size term) - 4 in
      let page_size = max 1 (term_height - 1) in
      let new_y = max 0 (state.selected_y - page_size) in
      let new_scroll_y = max 0 (min new_y state.scroll_y) in
      `Continue { state with selected_y = new_y; scroll_y = new_scroll_y }
  | `Key (`Page `Down, []) ->
      let filtered_packages = filter_packages state.packages state.compilers state.build_map state.selected_x state.table_filter state.search_text in
      let max_y = Array.length filtered_packages - 1 in
      let term_height = snd (NottyTerm.size term) - 4 in
      let page_size = max 1 (term_height - 1) in
      let new_y = min max_y (state.selected_y + page_size) in
      let new_scroll_y = if new_y >= state.scroll_y + term_height then new_y - term_height + 1 else state.scroll_y in
      `Continue { state with selected_y = new_y; scroll_y = new_scroll_y }
  | `Key (`Home, []) -> `Continue { state with selected_y = 0; scroll_y = 0 }
  | `Key (`End, []) ->
      let filtered_packages = filter_packages state.packages state.compilers state.build_map state.selected_x state.table_filter state.search_text in
      let max_y = Array.length filtered_packages - 1 in
      let term_height = snd (NottyTerm.size term) - 4 in
      let new_scroll_y = max 0 (max_y - term_height + 1) in
      `Continue { state with selected_y = max_y; scroll_y = new_scroll_y }
  | `Key (`Enter, []) -> (
      let filtered_packages = filter_packages state.packages state.compilers state.build_map state.selected_x state.table_filter state.search_text in
      if Array.length filtered_packages = 0 then
        `Continue state
      else
      let package = filtered_packages.(state.selected_y) in
      let compiler = state.compilers.(state.selected_x) in
      let key = { package; compiler } in
      (match Hashtbl.find_opt state.build_map key with
      | Some result ->
          (* If full data not loaded yet, load it and cache in hashtable *)
          let updated_state =
            if not state.full_data_loaded then (
              match state.column_indices with
              | Some column_indices ->
                  load_full_data state.current_filename state.build_map column_indices;
                  { state with full_data_loaded = true }
              | None -> state (* Skip if no column indices available *)
            ) else
              state
          in
          (* Now get the updated result with log/solution data *)
          let final_result =
            match Hashtbl.find_opt updated_state.build_map key with
            | Some r -> r
            | None -> result (* fallback *)
          in
          let log_lines =
            match final_result.log with
            | Some log when String.length log > 0 -> String.split_on_char '\n' (sanitize_text log)
            | _ -> []
          in
          let solution_lines =
            match final_result.solution with
            | Some solution when String.length solution > 0 -> String.split_on_char '\n' (sanitize_text solution)
            | _ -> []
          in
          let detail = { key; result = final_result; log_lines; solution_lines; detail_scroll = 0 } in
          `Continue { updated_state with mode = Detail_view detail }
      | None -> `Continue state))
  | `Key (`ASCII 'f', []) ->
      `Continue { state with table_filter = Filter_failed; selected_y = 0; scroll_y = 0 }
  | `Key (`ASCII 'n', []) ->
      `Continue { state with table_filter = Filter_no_solution; selected_y = 0; scroll_y = 0 }
  | `Key (`ASCII 'd', []) ->
      `Continue { state with table_filter = Filter_dependency_failed; selected_y = 0; scroll_y = 0 }
  | `Key (`ASCII 's', []) ->
      `Continue { state with table_filter = Filter_success; selected_y = 0; scroll_y = 0 }
  | `Key (`ASCII '/', []) ->
      `Continue { state with search_mode = true }
  | `Key (`ASCII 'c', []) ->
      `Continue { state with table_filter = No_filter; search_text = ""; selected_y = 0; scroll_y = 0 }
  | `Key (`ASCII 'q', [])
  | `Key (`Escape, []) ->
      let commits = get_git_commits state.opam_repo_path state.num_commits in
      let home = { commits; selected_commit = 0; scroll_offset = 0; selected_commits = [] } in
      `Continue { state with mode = Home_view home }
  | _ -> `Continue state

let rec event_loop term state =
  let img =
    match state.mode with
    | Home_view home -> draw_home_view home (NottyTerm.size term)
    | Table_view -> draw_table state (NottyTerm.size term)
    | Detail_view detail -> draw_detail_view detail (NottyTerm.size term)
    | Diff_view diff_info -> draw_diff_view diff_info (NottyTerm.size term)
  in
  NottyTerm.image term img;
  let event = NottyTerm.event term in
  let result =
    match (state.mode, event) with
    | Home_view home, event -> handle_home_event term state home event
    | Detail_view detail, event -> handle_detail_event term state detail event
    | Table_view, event -> handle_table_event term state event
    | Diff_view diff_info, event -> handle_diff_event term state diff_info event
  in
  match result with
  | `Continue new_state -> event_loop term new_state
  | `Quit -> ()
  | _ -> (
      match event with
      | `Resize _ -> event_loop term state
      | _ -> event_loop term state)

open Cmdliner

let main_cmd opam_repo_path num_commits fetch_flag =
  try
    Printf.printf "\n=== Starting TUI ===\n";
    Printf.printf "Using opam repository: %s\n" opam_repo_path;

    if fetch_flag then
      fetch_repository opam_repo_path;

    let commits = get_git_commits opam_repo_path num_commits in
    if List.is_empty commits then (
      Printf.printf "Error: No git commits found in %s\n" opam_repo_path;
      Printf.printf "Please ensure:\n";
      Printf.printf "1. The path points to a valid git repository\n";
      Printf.printf "2. You have read access to the repository\n";
      Printf.printf "3. The repository has commit history\n";
      exit 1);
    let term = NottyTerm.create () in
    let home = { commits; selected_commit = 0; scroll_offset = 0; selected_commits = [] } in
    let state =
      {
        opam_repo_path;
        num_commits;
        current_filename = "";  (* Will be set when a commit is selected *)
        build_map = Hashtbl.create 1000;
        full_data_loaded = false;
        packages = [||];
        compilers = [||];
        column_indices = None;
        scroll_x = 0;
        scroll_y = 0;
        selected_x = 0;
        selected_y = 0;
        table_filter = No_filter;
        search_mode = false;
        search_text = "";
        mode = Home_view home;
      }
    in

    event_loop term state;
    NottyTerm.release term
  with
  | exn -> Printf.printf "Error: %s\n" (Printexc.to_string exn)

let opam_repo_arg =
  let doc = "Path to the opam repository" in
  Arg.(value & pos 0 string (Sys.getenv "HOME" ^ "/opam-repository") & info [] ~docv:"PATH" ~doc)

let num_commits_arg =
  let doc = "Number of commits to show in the table" in
  Arg.(value & opt int 50 & info ["commits"] ~docv:"N" ~doc)

let fetch_arg =
  let doc = "Fetch latest changes from the remote repository before starting" in
  Arg.(value & flag & info ["fetch"] ~doc)

let cmd =
  let doc = "Terminal UI for browsing opam repository build results" in
  let info = Cmd.info "day10-tui" ~doc in
  Cmd.v info Term.(const main_cmd $ opam_repo_arg $ num_commits_arg $ fetch_arg)

let () = exit (Cmd.eval cmd)

open Notty
open Day10_tui_lib

(* open Notty.Infix *)
module Term = Notty_unix.Term

type commit_info = {
  sha : string;
  message : string;
  date : string;
  downloaded : bool;
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
}

type app_mode =
  | Home_view of home_info
  | Table_view
  | Detail_view of detail_info

type tui_state = {
  opam_repo_path : string;
  build_map : (build_key, build_result) Hashtbl.t;
  packages : string array;
  compilers : string array;
  scroll_x : int; (* horizontal scroll for compilers *)
  scroll_y : int; (* vertical scroll for packages *)
  selected_x : int; (* selected compiler column *)
  selected_y : int; (* selected package row *)
  mode : app_mode;
}

let get_git_commits opam_repo_path =
  let cmd = Stdlib.Filename.quote_command "git" [ "-C"; opam_repo_path; "log"; "--oneline"; "-n"; "50"; "--format=%H|%s|%ci" ] ~stderr:"/dev/null" in
  try
    let ic = Unix.open_process_in cmd in
    let result =
      try
        let rec read_lines acc =
          try
            let line = input_line ic in
            let parts = String.split_on_char '|' line in
            match parts with
            | [ sha; message; date ] ->
                let commit = { sha; message; date; downloaded = Stdlib.Sys.file_exists (sha ^ ".parquet") } in
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

let download_parquet sha =
  let filename = sha ^ ".parquet" in
  if not (Stdlib.Sys.file_exists filename) then
    let url = Printf.sprintf "https://www.cl.cam.ac.uk/~mte24/day10/%s.parquet" sha in
    let cmd = Stdlib.Filename.quote_command "curl" [ "-s"; "-f"; "-o"; filename; url ] ~stderr:"/dev/null" in
    let exit_code = Stdlib.Sys.command cmd in
    if exit_code = 0 && Stdlib.Sys.file_exists filename then `Success
    else (
      (* Clean up partial download *)
      if Stdlib.Sys.file_exists filename then Stdlib.Sys.remove filename;
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
  let table = Arrow.Parquet_reader.table filename in
  let name_col = Arrow.Wrapper.Column.read_utf8 table ~column:(`Name "name") in
  let status_col = Arrow.Wrapper.Column.read_utf8_opt table ~column:(`Name "status") in
  let compiler_col = Arrow.Wrapper.Column.read_utf8 table ~column:(`Name "compiler") in
  let log_col = Arrow.Wrapper.Column.read_utf8_opt table ~column:(`Name "log") in
  let solution_col = Arrow.Wrapper.Column.read_utf8_opt table ~column:(`Name "solution") in

  (* Package names and compilers are unique and non-null, data is likely pre-sorted *)
  let unique_packages = name_col in
  let unique_compilers = compiler_col in

  (* Create a lookup map: (package, compiler) -> (status, log, solution) *)
  let build_map = Hashtbl.create 1000 in
  Array.iteri (fun i package ->
      let compiler = compiler_col.(i) in
      match (status_col.(i), log_col.(i), solution_col.(i)) with
      | Some status, log, solution ->
          let key = { package; compiler } in
          let result = { status; log; solution } in
          Hashtbl.replace build_map key result
      | _ -> ()) name_col;

  (build_map, unique_packages, unique_compilers)

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

let draw_home_view { commits; selected_commit; scroll_offset } (w, h) =
  let list_items =
    List.map (fun commit ->
        let download_indicator = if commit.downloaded then Some "✓" else Some "○" in
        let short_sha = String.sub commit.sha 0 (min 8 (String.length commit.sha)) in
        let message_truncated = if String.length commit.message > 60 then String.sub commit.message 0 60 else commit.message in
        let display_text = Printf.sprintf "%s %s - %s" short_sha commit.date message_truncated in
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

let draw_table state (w, h) =
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

  let config =
    {
      Table_widget.Table.rows = state.packages;
      columns = state.compilers;
      get_cell;
      row_height = 1;
      column_width = 15;
      selected_row = Some state.selected_y;
      selected_col = Some state.selected_x;
      scroll_row = state.scroll_y;
      scroll_col = state.scroll_x;
      help_text = "Arrows: move, PgUp/PgDn: page, Home/End: first/last row, Enter: details, h: home, q: quit";
    }
  in

  Table_widget.Table.draw_table config (w, h)

let sanitize_text text =
  String.map (fun c ->
      let code = Char.code c in
      if code < 32 && code <> 10 then ' ' else c) text

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
      let content_height = snd (Term.size term) - 3 in
      let new_scroll = if new_selected >= home.scroll_offset + content_height then new_selected - content_height + 1 else home.scroll_offset in
      let new_home = { home with selected_commit = new_selected; scroll_offset = new_scroll } in
      `Continue { state with mode = Home_view new_home }
  | `Key (`Page `Up, []) ->
      let content_height = snd (Term.size term) - 3 in
      let page_size = max 1 (content_height - 1) in
      let new_selected = max 0 (home.selected_commit - page_size) in
      let new_scroll = max 0 (min new_selected home.scroll_offset) in
      let new_home = { home with selected_commit = new_selected; scroll_offset = new_scroll } in
      `Continue { state with mode = Home_view new_home }
  | `Key (`Page `Down, []) ->
      let max_idx = List.length home.commits - 1 in
      let content_height = snd (Term.size term) - 3 in
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
      let content_height = snd (Term.size term) - 3 in
      let new_scroll = max 0 (max_idx - content_height + 1) in
      let new_home = { home with selected_commit = max_idx; scroll_offset = new_scroll } in
      `Continue { state with mode = Home_view new_home }
  | `Key (`Enter, []) -> (
      match List.nth_opt home.commits home.selected_commit with
      | Some commit -> (
          let filename = commit.sha ^ ".parquet" in
          match download_parquet commit.sha with
          | `Success -> (
              try
                let build_map, packages, compilers = analyze_data filename in
                let new_state = { state with build_map; packages; compilers; mode = Table_view } in
                `Continue new_state
              with
              | exn ->
                  let raw_error = Printexc.to_string exn in
                  let clean_error = sanitize_text raw_error in
                  let error_msg = Printf.sprintf "Failed to parse parquet file: %s" clean_error in
                  show_error_message term error_msg;
                  `Continue state)
          | `Error error_msg ->
              show_error_message term error_msg;
              `Continue state)
      | None -> `Continue state)
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
      let max_scroll = max 0 (total_lines - (snd (Term.size term) - 3)) in
      let new_scroll = min max_scroll (detail.detail_scroll + 1) in
      let new_detail = { detail with detail_scroll = new_scroll } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`Page `Up, []) ->
      let content_height = snd (Term.size term) - 3 in
      let page_size = max 1 (content_height - 1) in
      let new_scroll = max 0 (detail.detail_scroll - page_size) in
      let new_detail = { detail with detail_scroll = new_scroll } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`Page `Down, []) ->
      let total_lines = List.length detail.log_lines + List.length detail.solution_lines + 4 in
      let max_scroll = max 0 (total_lines - (snd (Term.size term) - 3)) in
      let content_height = snd (Term.size term) - 3 in
      let page_size = max 1 (content_height - 1) in
      let new_scroll = min max_scroll (detail.detail_scroll + page_size) in
      let new_detail = { detail with detail_scroll = new_scroll } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`Home, []) ->
      let new_detail = { detail with detail_scroll = 0 } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`End, []) ->
      let total_lines = List.length detail.log_lines + List.length detail.solution_lines + 4 in
      let max_scroll = max 0 (total_lines - (snd (Term.size term) - 3)) in
      let new_detail = { detail with detail_scroll = max_scroll } in
      `Continue { state with mode = Detail_view new_detail }
  | `Key (`Escape, [])
  | `Key (`ASCII 'q', []) ->
      `Continue { state with mode = Table_view }
  | _ -> `Continue state

let handle_table_event term state event =
  match event with
  | `Key (`ASCII 'h', []) ->
      let commits = get_git_commits state.opam_repo_path in
      let home = { commits; selected_commit = 0; scroll_offset = 0 } in
      `Continue { state with mode = Home_view home }
  | `Key (`Arrow `Up, []) ->
      let new_y = max 0 (state.selected_y - 1) in
      let new_scroll_y = if new_y < state.scroll_y then new_y else state.scroll_y in
      `Continue { state with selected_y = new_y; scroll_y = new_scroll_y }
  | `Key (`Arrow `Down, []) ->
      let max_y = Array.length state.packages - 1 in
      let new_y = min max_y (state.selected_y + 1) in
      let term_height = snd (Term.size term) - 4 in
      let new_scroll_y = if new_y >= state.scroll_y + term_height then new_y - term_height + 1 else state.scroll_y in
      `Continue { state with selected_y = new_y; scroll_y = new_scroll_y }
  | `Key (`Arrow `Left, []) ->
      let new_x = max 0 (state.selected_x - 1) in
      let new_scroll_x = if new_x < state.scroll_x then new_x else state.scroll_x in
      `Continue { state with selected_x = new_x; scroll_x = new_scroll_x }
  | `Key (`Arrow `Right, []) ->
      let max_x = Array.length state.compilers - 1 in
      let new_x = min max_x (state.selected_x + 1) in
      let term_width = fst (Term.size term) in
      let visible_compilers = (term_width - 30) / 15 in
      let new_scroll_x = if new_x >= state.scroll_x + visible_compilers then new_x - visible_compilers + 1 else state.scroll_x in
      `Continue { state with selected_x = new_x; scroll_x = new_scroll_x }
  | `Key (`Page `Up, []) ->
      let term_height = snd (Term.size term) - 4 in
      let page_size = max 1 (term_height - 1) in
      let new_y = max 0 (state.selected_y - page_size) in
      let new_scroll_y = max 0 (min new_y state.scroll_y) in
      `Continue { state with selected_y = new_y; scroll_y = new_scroll_y }
  | `Key (`Page `Down, []) ->
      let max_y = Array.length state.packages - 1 in
      let term_height = snd (Term.size term) - 4 in
      let page_size = max 1 (term_height - 1) in
      let new_y = min max_y (state.selected_y + page_size) in
      let new_scroll_y = if new_y >= state.scroll_y + term_height then new_y - term_height + 1 else state.scroll_y in
      `Continue { state with selected_y = new_y; scroll_y = new_scroll_y }
  | `Key (`Home, []) -> `Continue { state with selected_y = 0; scroll_y = 0 }
  | `Key (`End, []) ->
      let max_y = Array.length state.packages - 1 in
      let term_height = snd (Term.size term) - 4 in
      let new_scroll_y = max 0 (max_y - term_height + 1) in
      `Continue { state with selected_y = max_y; scroll_y = new_scroll_y }
  | `Key (`Enter, []) -> (
      let package = state.packages.(state.selected_y) in
      let compiler = state.compilers.(state.selected_x) in
      let key = { package; compiler } in
      (match Hashtbl.find_opt state.build_map key with
      | Some result ->
          let log_lines =
            match result.log with
            | Some log when String.length log > 0 -> String.split_on_char '\n' (sanitize_text log)
            | _ -> []
          in
          let solution_lines =
            match result.solution with
            | Some solution when String.length solution > 0 -> String.split_on_char '\n' (sanitize_text solution)
            | _ -> []
          in
          let detail = { key; result; log_lines; solution_lines; detail_scroll = 0 } in
          `Continue { state with mode = Detail_view detail }
      | None -> `Continue state))
  | `Key (`ASCII 'q', [])
  | `Key (`Escape, []) ->
      `Quit
  | _ -> `Continue state

let rec event_loop term state =
  let img =
    match state.mode with
    | Home_view home -> draw_home_view home (Term.size term)
    | Table_view -> draw_table state (Term.size term)
    | Detail_view detail -> draw_detail_view detail (Term.size term)
  in
  Term.image term img;
  let event = Term.event term in
  let result =
    match (state.mode, event) with
    | Home_view home, event -> handle_home_event term state home event
    | Detail_view detail, event -> handle_detail_event term state detail event
    | Table_view, event -> handle_table_event term state event
  in
  match result with
  | `Continue new_state -> event_loop term new_state
  | `Quit -> ()
  | _ -> (
      match event with
      | `Resize _ -> event_loop term state
      | _ -> event_loop term state)

let () =
  let opam_repo_path = if Array.length Stdlib.Sys.argv > 1 then Stdlib.Sys.argv.(1) else Stdlib.Sys.getenv "HOME" ^ "/opam-repository" in

  try
    Printf.printf "\n=== Starting TUI ===\n";
    Printf.printf "Using opam repository: %s\n" opam_repo_path;

    let commits = get_git_commits opam_repo_path in
    if List.is_empty commits then (
      Printf.printf "Error: No git commits found in %s\n" opam_repo_path;
      Printf.printf "Please ensure:\n";
      Printf.printf "1. The path points to a valid git repository\n";
      Printf.printf "2. You have read access to the repository\n";
      Printf.printf "3. The repository has commit history\n";
      Stdlib.exit 1);
    let term = Term.create () in
    let home = { commits; selected_commit = 0; scroll_offset = 0 } in
    let state =
      {
        opam_repo_path;
        build_map = Hashtbl.create 1000;
        packages = [||];
        compilers = [||];
        scroll_x = 0;
        scroll_y = 0;
        selected_x = 0;
        selected_y = 0;
        mode = Home_view home;
      }
    in

    event_loop term state;
    Term.release term
  with
  | exn -> Printf.printf "Error: %s\n" (Printexc.to_string exn)

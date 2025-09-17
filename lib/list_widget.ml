module A = Notty.A
module I = Notty.I

(* Generic scrollable list widget *)
module List = struct
  type 'a list_item = {
    content : 'a;
    display_text : string;
    status_indicator : string option; (* Optional status like ✓, ○, etc. *)
    attr : Notty.attr;
  }

  type 'a list_config = {
    items : 'a list_item list;
    selected_item : int;
    scroll_offset : int;
    title : string;
    help_text : string;
  }

  let draw_list config (w, h) =
    let header = I.string A.(fg white ++ st bold) config.title in
    let content_height = h - 3 in
    (* Reserve space for header and help *)

    let visible_items =
      let rec drop n lst = match n, lst with
        | 0, _ | _, [] -> lst
        | n, _ :: tl -> drop (n-1) tl in
      let rec take n lst = match n, lst with
        | 0, _ | _, [] -> []
        | n, hd :: tl -> hd :: take (n-1) tl in
      drop config.scroll_offset config.items |> take content_height in

    let item_lines =
      List.mapi (fun i item ->
          let idx = config.scroll_offset + i in
          let selected = idx = config.selected_item in
          let attr = if selected then A.(fg black ++ bg white) else item.attr in

          let line_text =
            match item.status_indicator with
            | Some indicator -> Printf.sprintf "%s %s" indicator item.display_text
            | None -> item.display_text
          in

          (* Truncate to fit terminal width with some margin *)
          let truncated =
            let max_len = max 10 (w - 4) in
            if String.length line_text <= max_len then line_text
            else String.sub line_text 0 max_len in
          I.string attr truncated) visible_items
    in

    let content = I.vcat item_lines in
    let help_msg = I.string A.(fg black ++ bg white) config.help_text in

    let full_view = I.(header <-> I.void 0 1 <-> content <-> I.void 0 1 <-> help_msg) in
    I.crop ~l:0 ~t:0 ~r:(max 0 (I.width full_view - w)) ~b:(max 0 (I.height full_view - h)) full_view
end

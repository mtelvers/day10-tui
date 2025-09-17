module A = Notty.A
module I = Notty.I

(* Generic scrollable text viewer widget *)
module TextViewer = struct
  type text_section = {
    title : string;
    title_attr : Notty.attr;
    lines : string list;
    line_attr : Notty.attr;
  }

  type viewer_config = {
    header : string;
    header_attr : Notty.attr;
    sections : text_section list;
    scroll_offset : int;
    help_text : string;
  }

  let draw_viewer config (w, h) =
    let header_img = I.string config.header_attr config.header in

    let all_lines =
      List.fold_left (fun acc section ->
          let section_header = I.string section.title_attr section.title in
          let section_content =
            if section.lines = [] then [ I.string A.(fg yellow) ("No " ^ String.lowercase_ascii section.title ^ " available") ]
            else List.map (I.string section.line_attr) section.lines
          in
          let spacing = if acc = [] then [] else [ I.void 0 1 ] in
          acc @ spacing @ [ section_header ] @ section_content) [] config.sections
    in

    let content_height = h - 3 in
    (* Reserve space for header and help *)
    let rec drop n lst = match n, lst with | 0, _ | _, [] -> lst | n, _ :: tl -> drop (n-1) tl in
    let rec take n lst = match n, lst with | 0, _ | _, [] -> [] | n, hd :: tl -> hd :: take (n-1) tl in
    let visible_lines = drop config.scroll_offset all_lines |> take content_height in
    let content = I.vcat visible_lines in

    let help_msg = I.string A.(fg black ++ bg white) config.help_text in

    let full_view = I.(header_img <-> I.void 0 1 <-> content <-> I.void 0 1 <-> help_msg) in
    I.crop ~l:0 ~t:0 ~r:(max 0 (I.width full_view - w)) ~b:(max 0 (I.height full_view - h)) full_view
end

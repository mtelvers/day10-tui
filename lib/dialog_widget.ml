open Base
module A = Notty.A
module I = Notty.I

(* Generic modal dialog widget *)
module Dialog = struct
  type dialog_type =
    | Error
    | Info
    | Warning
    | Confirm

  type dialog_config = {
    dialog_type : dialog_type;
    title : string;
    message : string;
    help_text : string;
  }

  let dialog_colors = function
    | Error -> A.(fg red ++ st bold)
    | Info -> A.(fg blue ++ st bold)
    | Warning -> A.(fg yellow ++ st bold)
    | Confirm -> A.(fg cyan ++ st bold)

  let dialog_title = function
    | Error -> "Error"
    | Info -> "Information"
    | Warning -> "Warning"
    | Confirm -> "Confirm"

  let sanitize_text text =
    (* Remove all control characters (including tabs and newlines) and replace with spaces *)
    String.map text ~f:(fun c ->
        let code = Char.to_int c in
        if code < 32 then ' ' (* Replace all control chars with spaces *) else c)

  let draw_dialog config (w, h) =
    let title_attr = dialog_colors config.dialog_type in
    let title_text =
      match config.title with
      | "" -> dialog_title config.dialog_type
      | custom -> custom
    in

    let header = I.string title_attr title_text in

    (* Sanitize the message first *)
    let clean_message = sanitize_text config.message in

    (* Split message into lines if it's too long *)
    let message_width = max 20 (min 60 (w - 4)) in
    let message_lines =
      if String.length clean_message <= message_width then [ clean_message ]
      else
        let words = String.split clean_message ~on:' ' in
        let fold_word (lines, current_line) word =
          let new_line = if String.is_empty current_line then word else current_line ^ " " ^ word in
          if String.length new_line <= message_width then (lines, new_line)
          else if String.is_empty current_line then ([ word ], "") (* Handle very long single words *)
          else (lines @ [ current_line ], word)
        in
        let lines, final_line = List.fold words ~init:([], "") ~f:fold_word in
        if String.is_empty final_line then lines else lines @ [ final_line ]
    in

    let message_imgs = List.map message_lines ~f:(I.string A.(fg white)) in
    let message = I.vcat message_imgs in

    let help = I.string A.(fg black ++ bg white) (sanitize_text config.help_text) in
    let dialog_view = I.(header <-> I.void 0 1 <-> message <-> I.void 0 1 <-> help) in

    let cropped = I.crop ~l:0 ~t:0 ~r:(max 0 (I.width dialog_view - w)) ~b:(max 0 (I.height dialog_view - h)) dialog_view in
    cropped

  let show_dialog term config =
    let w, h = Notty_unix.Term.size term in
    let dialog_img = draw_dialog config (w, h) in
    Notty_unix.Term.image term dialog_img;
    ignore (Notty_unix.Term.event term)
end

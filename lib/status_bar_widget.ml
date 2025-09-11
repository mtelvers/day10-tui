open Base
module A = Notty.A
module I = Notty.I

(* Generic status bar widget *)
module StatusBar = struct
  type status_config = {
    left_text : string;
    right_text : string;
    attr : Notty.attr;
  }

  let draw_status_bar config width =
    let combined_text = config.left_text ^ " | " ^ config.right_text in
    let truncated = String.prefix combined_text (max 10 (width - 2)) in
    I.string config.attr truncated

  let create_default_status ~position ~help = { left_text = position; right_text = help; attr = A.(fg black ++ bg white) }
end

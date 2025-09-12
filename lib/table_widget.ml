open Base
module A = Notty.A
module I = Notty.I

(* Generic table widget that can display any 2D data *)
module Table = struct
  type cell_content = {
    text : string;
    attr : Notty.attr;
  }

  type 'a table_config = {
    rows : 'a list;
    columns : string list;
    get_cell : row:'a -> column:string -> cell_content;
    row_height : int;
    column_width : int;
    selected_row : int option;
    selected_col : int option;
    scroll_row : int;
    scroll_col : int;
    help_text : string;
  }

  let padding str width attr =
    let img = I.string attr str in
    let str_width = I.width img in
    let total_pad = max 0 (width - str_width) in
    let left_pad = total_pad / 2 in
    let right_pad = total_pad - left_pad in
    I.hpad left_pad right_pad img

  let draw_table config (w, h) =
    let header_height = 3 in
    let available_height = h - header_height in
    let available_width = w in

    (* Calculate visible columns and rows *)
    let package_width = 30 in
    let visible_columns = min (List.length config.columns) ((available_width - package_width) / config.column_width) in
    let visible_rows = min (List.length config.rows) available_height in

    let visible_column_list = List.drop config.columns config.scroll_col |> fun l -> List.take l visible_columns in

    let visible_row_list = List.drop config.rows config.scroll_row |> fun l -> List.take l visible_rows in

    (* Create header *)
    let column_headers =
      List.mapi visible_column_list ~f:(fun i column ->
          let selected =
            match config.selected_col with
            | Some sel_col -> i + config.scroll_col = sel_col
            | None -> false
          in
          let attr = if selected then A.(fg black ++ bg white) else A.(fg cyan) in
          (* Extract version after first dot, or use full name if no dot *)
          let display_name =
            match String.index column '.' with
            | Some dot_pos -> String.drop_prefix column (dot_pos + 1)
            | None -> column
          in
          padding display_name config.column_width attr)
    in

    let row_header = I.string A.(fg white ++ st bold) "Package" |> I.hpad (package_width - 7) 0 in
    (* "Package" is 7 chars, left-aligned *)

    let header = List.fold column_headers ~init:row_header ~f:I.( <|> ) in

    (* Create rows *)
    let data_rows =
      List.mapi visible_row_list ~f:(fun i row ->
          let selected_row =
            match config.selected_row with
            | Some sel_row -> i + config.scroll_row = sel_row
            | None -> false
          in

          (* First cell is the row identifier *)
          let row_name = config.get_cell ~row ~column:"_row_name" in
          let row_attr = if selected_row then A.(fg black ++ bg white) else row_name.attr in
          let truncated_row = String.prefix row_name.text (package_width - 1) in
          let row_cell = I.string row_attr truncated_row |> I.hpad (package_width - String.length truncated_row) 0 in

          let data_cells =
            List.mapi visible_column_list ~f:(fun j column ->
                let selected_cell =
                  selected_row
                  &&
                  match config.selected_col with
                  | Some sel_col -> j + config.scroll_col = sel_col
                  | None -> false
                in
                let cell_data = config.get_cell ~row ~column in
                let bg_attr = if selected_cell then A.(bg white) else A.empty in
                let final_attr = A.(cell_data.attr ++ bg_attr) in
                padding cell_data.text config.column_width final_attr)
          in

          List.fold data_cells ~init:row_cell ~f:I.( <|> ))
    in

    let table = List.fold data_rows ~init:header ~f:I.( <-> ) in

    (* Add help bar *)
    let help_bar = I.string A.(fg black ++ bg white) config.help_text in

    let final_table = I.(table <-> I.void 0 1 <-> help_bar) in
    I.crop ~l:0 ~t:0 ~r:(max 0 (I.width final_table - w)) ~b:(max 0 (I.height final_table - h)) final_table
end

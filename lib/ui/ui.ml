open Notty
open Notty_unix

let draw_input input_str width_ ~cur_offset =
  let str_len = String.length input_str in
  let numl x =
    int_of_float @@ ceil @@ (float_of_int x /. float_of_int width_)
  in
  let lines =
    List.init (numl str_len) (fun i ->
      String.sub input_str (i * width_) @@ min width_ (str_len - (i * width_)))
  in
  let cur_x = cur_offset mod width_ in
  let bkg = I.char A.empty ' ' width_ (List.length lines) in
  ( (cur_x, str_len / width_)
  , I.(
      char A.empty '-' width_ 1
      <-> (vcat @@ List.map (fun s -> string A.(fg green) s) lines </> bkg)) )
;;

let draw_table projects ~state =
  List.fold_left
    (fun (idx, name_col, desc_col, tag_col) (project : Db.Project.t) ->
      let selected = idx = State.get_row state in
      let selected_attr =
        if selected then A.(st bold ++ st underline) else A.empty
      in
      let desc_sty, tag_sty =
        match State.current_col state with
        | Description -> A.(fg blue ++ selected_attr), A.(fg blue)
        | Tags -> A.(fg blue), A.(fg blue ++ selected_attr)
      in
      let tags_str = String.concat ", " project.tags in
      let name_col = I.(name_col <-> string A.(fg blue) project.dirname) in
      let desc_col = I.(desc_col <-> string desc_sty project.description) in
      let tag_col = I.(tag_col <-> string tag_sty tags_str) in
      idx + 1, name_col, desc_col, tag_col)
    (0, I.empty, I.empty, I.empty)
    projects
;;

let draw (w, h) ~state ~term =
  let _, a, b, c = draw_table ~state @@ State.get_n_projects state h in
  let divider = I.(hpad 1 1 @@ char A.(st bold) '|' 1 (height a)) in
  let table = I.hcat [ a; divider; b; divider; c ] in
  match State.get_edit state with
  | None -> table
  | Some s ->
    let (x, y), input =
      draw_input
        (State.EditState.get_text s)
        w
        ~cur_offset:(State.EditState.cursor_offset s)
    in
    Term.cursor term (Some (x, y + h - I.height input + 1));
    I.(pad ~t:(h - height input) @@ input </> table)
;;

let rec update t state =
  Term.image t (draw (Term.size t) ~state ~term:t);
  loop t state

and loop t state =
  match State.get_edit state with
  | Some s ->
    (match Term.event t with
     | `Key (`Escape, _) | `Key (`Enter, _) ->
       Term.cursor t None;
       update t @@ State.commit_edit state
     | `Key (`ASCII c, _) ->
       update t @@ State.update_edit state @@ State.EditState.insert_char c s
     | `Key (`Backspace, _) ->
       update t @@ State.update_edit state @@ State.EditState.delete_char s
     | `Key (`Arrow `Left, _) ->
       update t @@ State.update_edit state @@ State.EditState.left s
     | `Key (`Arrow `Right, _) ->
       update t @@ State.update_edit state @@ State.EditState.right s
     | _ -> loop t state)
  | None ->
    (match Term.event t with
     | `Key (`ASCII 'q', _) -> ()
     | `Key (`ASCII 'J', _) -> update t @@ State.scroll_down state
     | `Key (`ASCII 'K', _) -> update t @@ State.scroll_up state
     | `Key (`ASCII 'j', _) | `Key (`Arrow `Down, _) ->
       update t @@ State.down state
     | `Key (`ASCII 'k', _) | `Key (`Arrow `Up, _) -> update t @@ State.up state
     | `Key (`ASCII 'h', _) | `Key (`Arrow `Left, _) ->
       update t @@ State.left state
     | `Key (`ASCII 'l', _) | `Key (`Arrow `Right, _) ->
       update t @@ State.right state
     | `Key (`Enter, _) -> update t @@ State.start_edit state
     | `Resize (w, h) -> update t @@ State.set_window_size w h state
     | _ -> loop t state)
;;

let start () =
  let t = Term.create () in
  update t @@ State.init ~window_size:(Term.size t) ();
  Term.release t
;;

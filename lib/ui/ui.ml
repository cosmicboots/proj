open Notty
open Notty_unix

let draw_table projects ~state =
  List.fold_left
    (fun (idx, name_col, desc_col, tag_col) (project : Db.Project.t) ->
      let selected_attr =
        if idx = State.get_row state
        then A.(st bold ++ st underline)
        else A.empty
      in
      let name_col =
        I.(name_col <-> string A.(fg blue ++ selected_attr) project.dirname)
      in
      let desc_col =
        I.(desc_col <-> string A.(fg blue ++ selected_attr) project.description)
      in
      let tag_col =
        I.(tag_col <-> string A.(fg blue) @@ String.concat ", " project.tags)
      in
      idx + 1, name_col, desc_col, tag_col)
    (0, I.empty, I.empty, I.empty)
    projects
;;

let draw (_w, h) state =
  let _, a, b, c = draw_table ~state @@ State.get_n_projects state h in
  let divider = I.(hpad 1 1 @@ char A.(st bold) '|' 1 (height a)) in
  I.(hcat [ a; divider; b; divider; c ])
;;

let rec update t state =
  Term.image t (draw (Term.size t) state);
  loop t state

and loop t state =
  match Term.event t with
  | `Key (`ASCII 'q', _) -> ()
  | `Key (`ASCII 'J', _) -> update t @@ State.scroll_down state
  | `Key (`ASCII 'K', _) -> update t @@ State.scroll_up state
  | `Key (`ASCII 'j', _) -> update t @@ State.down state
  | `Key (`ASCII 'k', _) -> update t @@ State.up state
  | `Resize (w, h) -> update t @@ State.set_window_size w h state
  | _ -> loop t state
;;

let start () =
  let t = Term.create () in
  update t @@ State.init ~window_size:(Term.size t) ();
  Term.release t
;;

open Base

type t =
  { projects : Db.Project.t list
  ; starting_row : int
  ; selected_row : int
  ; window_size : int * int
  }

let projects () =
  List.init 100 ~f:(fun x ->
    Db.Project.
      { id = x
      ; dirname = Stdlib.Format.sprintf "Test %d" x
      ; description = "description test"
      ; tags = [ "asdf"; "asdf" ]
      })
;;

let get_projects s = s.projects
let get_n_projects s n = List.take (List.drop s.projects s.starting_row) n
let get_row s = s.selected_row

let scroll_down s =
  { s with
    starting_row = min (s.starting_row + 1) (List.length s.projects - 1)
  }
;;

let scroll_up s = { s with starting_row = max (s.starting_row - 1) 0 }

let down s =
  let new_pos = min (s.selected_row + 1) (snd s.window_size) in
  if new_pos >= snd s.window_size
  then scroll_down s
  else { s with selected_row = new_pos }
;;

let up s =
  if s.selected_row = 0
  then scroll_up s
  else { s with selected_row = max (s.selected_row - 1) 0 }
;;

let init ?(window_size = 0, 0) () =
  { projects = projects (); selected_row = 0; starting_row = 0; window_size }
;;

let set_window_size w h s =
  { s with window_size = w, h; selected_row = s.selected_row |> min (h - 1) }
;;

open Base

type column =
  | Description
  | Tags

module EditState = struct
  type t =
    { new_value : string
    ; cursor_offset : int
    }

  let init new_value = { new_value; cursor_offset = String.length new_value }
  let get_text s = s.new_value
  let cursor_offset s = s.cursor_offset

  let insert_char c s =
    let new_value =
      String.sub s.new_value ~pos:0 ~len:s.cursor_offset
      ^ String.make 1 c
      ^ String.sub
          s.new_value
          ~pos:s.cursor_offset
          ~len:(String.length s.new_value - s.cursor_offset)
    in
    { cursor_offset = s.cursor_offset + 1; new_value }
  ;;

  let delete_char s =
    if String.length s.new_value = 0
    then s
    else (
      let new_value =
        String.sub s.new_value ~pos:0 ~len:(max 0 @@ (s.cursor_offset - 1))
        ^ String.sub
            s.new_value
            ~pos:s.cursor_offset
            ~len:(String.length s.new_value - s.cursor_offset)
      in
      { cursor_offset = s.cursor_offset - 1; new_value })
  ;;

  let right s =
    { s with
      cursor_offset = min (s.cursor_offset + 1) (String.length s.new_value)
    }
  ;;

  let left s = { s with cursor_offset = max (s.cursor_offset - 1) 0 }
end

type t =
  { projects : Db.Project.t list
  ; starting_row : int
  ; selected_row : int
  ; window_size : int * int
  ; selected_column : column
  ; edit_state : EditState.t option
  }

let projects () =
  List.init 100 ~f:(fun x ->
    Db.Project.
      { id = x
      ; dirname = Stdlib.Format.sprintf "Test %d" x
      ; description = "description test" ^ Int.to_string x
      ; tags = [ "asdf"; "asdf" ]
      })
;;

let get_projects s = s.projects
let get_n_projects s n = List.take (List.drop s.projects s.starting_row) n
let get_row s = s.selected_row
let current_col s = s.selected_column

let start_edit s =
  let project = List.nth_exn s.projects (s.selected_row + s.starting_row) in
  let cval =
    match s.selected_column with
    | Description -> project.description
    | Tags -> String.concat ~sep:"," project.tags
  in
  { s with edit_state = Some (EditState.init cval) }
;;

let get_edit s = s.edit_state
let update_edit s e = { s with edit_state = Some e }
let commit_edit s = (* TODO *) { s with edit_state = None }

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

let left s =
  { s with
    selected_column =
      (match s.selected_column with
       | Description -> Description
       | Tags -> Description)
  }
;;

let right s =
  { s with
    selected_column =
      (match s.selected_column with
       | Description -> Tags
       | Tags -> Tags)
  }
;;

let init ?(window_size = 0, 0) () =
  { projects = projects ()
  ; selected_row = 0
  ; starting_row = 0
  ; window_size
  ; selected_column = Description
  ; edit_state = None
  }
;;

let set_window_size w h s =
  { s with window_size = w, h; selected_row = s.selected_row |> min (h - 1) }
;;

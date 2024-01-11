type t

module EditState : sig
  type t

  val init : string -> t

  (** [get_text s] returns the text in the edit state [s]. *)
  val get_text : t -> string

  (** [set_cursor_offset s n] returns the state [s] with the cursor offset
      set to [n]. *)
  val cursor_offset : t -> int

  (** [insert_char c s] returns the state [s] with the character [c] inserted
      at the cursor position. *)
  val insert_char : char -> t -> t

  (** [delete_char s] returns the state [s] with the character at the cursor
      position deleted. *)
  val delete_char : t -> t

  (** [right s] returns the state [s] with the cursor moved right. *)
  val right : t -> t

  (** [left s] returns the state [s] with the cursor moved left. *)
  val left : t -> t
end

type column =
  | Description
  | Tags

(** [get_window_size ()] returns the size of the window. *)
val set_window_size : int -> int -> t -> t

val current_col : t -> column
val start_edit : t -> t
val get_edit : t -> EditState.t option
val update_edit : t -> EditState.t -> t

(** [update_projects s ps] returns the state [s] with the projects set to [ps]. *)
val update_projects : t -> Db.Project.t list -> t

(** [commit_edit s] returns the state [s] with the edit state committed to the
    db. *)
val commit_edit
  :  t
  -> (module Caqti_lwt.CONNECTION)
  -> (t, [> Caqti_error.call_or_retrieve ]) Lwt_result.t

(** [get_projects s] returns the list of all projects in [s]. *)
val get_projects : t -> Db.Project.t list

(** [get_n_projects s n] returns the list of the [n] next projects in [s]. *)
val get_n_projects : t -> int -> Db.Project.t list

(** [scroll_down s] returns the state [s] with the view moved down. *)
val scroll_down : t -> t

(** [scroll_up s] returns the state [s] with the view moved up. *)
val scroll_up : t -> t

val get_row : t -> int

(** [down s] returns the state [s] with the cursor moved down. *)
val down : t -> t

(** [up s] returns the state [s] with the cursor moved up. *)
val up : t -> t

val left : t -> t
val right : t -> t

(** Get an initial state. *)
val init : ?window_size:int * int -> Db.Project.t list -> t

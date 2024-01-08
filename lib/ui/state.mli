type t

(** [get_window_size ()] returns the size of the window. *)
val set_window_size : int -> int -> t -> t

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

(** Get an initial state. *)
val init : ?window_size:int * int -> unit -> t

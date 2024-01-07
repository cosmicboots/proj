type t =
  { id : int
  ; dirname : string
  ; description : string
  ; tags : string list
  }

(** [insert ~dirname ~description ~tags db] inserts a new project into the
    database [db]. *)
val insert
  :  dirname:string
  -> ?description:string
  -> ?tags:string list
  -> (module Caqti_lwt.CONNECTION)
  -> (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

(** [list db] returns the list of all projects in the database [db]. *)
val list
  :  (module Caqti_lwt.CONNECTION)
  -> (t list, [> Caqti_error.call_or_retrieve ]) Lwt_result.t

(** [get ~id db] returns the project with the given [id] in the database [db]. *)
val get
  :  id:int
  -> (module Caqti_lwt.CONNECTION)
  -> (t option, [> Caqti_error.call_or_retrieve ]) Lwt_result.t

(** [update ~id ?dirname ?description ?tags db] updates the project with the
    given [id] in the database [db]. *)
val update
  :  dirname:string
  -> ?description:string
  -> ?tags:string list
  -> (module Caqti_lwt.CONNECTION)
  -> (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

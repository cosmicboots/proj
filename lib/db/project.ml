let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

type t =
  { id : int
  ; dirname : string
  ; description : string
  ; tags : string list
  }

let t_of_tuple (id, (dirname, (description, (tags, ())))) =
  { id; dirname; description; tags = String.split_on_char ',' tags }
;;

let project_table, (Expr.[ id; dirname; description; tags ] as all_fields) =
  StaticSchema.declare_table
    schema
    ~name:"project"
    Schema.
      [ field
          ~constraints:[ primary_key ~auto_increment:true () ]
          "id"
          ~ty:Type.int
      ; field "dirname" ~constraints:[ unique (); not_null () ] ~ty:Type.text
      ; field "description" ~ty:Type.text ~constraints:[ not_null () ]
      ; field "tags" ~ty:Type.text ~constraints:[ not_null () ]
      ]
;;

let insert ~dirname:dirname_ ?description:(desc_ = "") ?tags:(tags_ = []) db =
  Query.insert
    ~table:project_table
    ~values:
      Expr.
        [ dirname := s dirname_
        ; description := s desc_
        ; tags := s @@ String.concat " " tags_
        ]
  |> Request.make_zero
  |> Petrol.exec db
;;

let list db =
  let open Lwt_result.Infix in
  Query.select all_fields ~from:project_table
  |> Request.make_many
  |> Petrol.collect_list db
  >|= List.map t_of_tuple
;;

let get ~id:id_ db =
  let open Lwt_result.Infix in
  Query.select all_fields ~from:project_table
  |> Query.where Expr.(id = i id_)
  |> Request.make_one
  |> Petrol.find_opt db
  >|= Option.map t_of_tuple
;;

let update ~dirname:dirname_ ?description:desc_ ?tags:tags_ db =
  let update_expr = ref [] in
  List.iter
    (fun y ->
      Option.iter (fun x ->
        update_expr := Expr.((fst y := s x) :: !update_expr))
      @@ snd y)
    [ description, desc_; tags, Option.map (String.concat ",") tags_ ];
  Query.update ~table:project_table ~set:!update_expr
  |> Query.where Expr.(dirname = s dirname_)
  |> Request.make_zero
  |> Petrol.exec db
;;

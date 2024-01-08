open Minttea
open Db

type column =
  | Description
  | Tags

type mode =
  { projects : Project.t list
  ; cursor : int * column
  }

let initial_model () =
  { projects =
      Project.
        [ { id = 0; dirname = "test"; description = "test"; tags = [ "test" ] }
        ; { id = 1; dirname = "test2"; description = "test"; tags = [ "test" ] }
        ; { id = 2; dirname = "test3"; description = "test"; tags = [ "test" ] }
        ]
  ; cursor = 0, Description
  }
;;

let init _model = Command.Noop

let update event model =
  match event with
  | Event.KeyDown "q" -> model, Command.Quit
  | Event.KeyDown ("j" | "down") ->
    ( { model with
        cursor =
          ( min (fst model.cursor + 1) (List.length model.projects - 1)
          , snd model.cursor )
      }
    , Command.Noop )
  | Event.KeyDown ("k" | "up") ->
    ( { model with cursor = max (fst model.cursor - 1) 0, snd model.cursor }
    , Command.Noop )
  | Event.KeyDown ("h" | "left") ->
    { model with cursor = fst model.cursor, Description }, Command.Noop
  | Event.KeyDown ("l" | "right") ->
    { model with cursor = fst model.cursor, Tags }, Command.Noop
  | _ -> model, Command.Noop
;;

let view model =
  let c1 = ref 7 in
  let c2 = ref 11 in
  let c3 = ref 4 in
  let table =
    List.mapi
      (fun idx p ->
        let ((a, b, c) as res) =
          Format.(
            ( sprintf "%s" p.Db.Project.dirname
            , sprintf
                (if snd model.cursor = Description && idx = fst model.cursor
                 then ">%s<"
                 else "%s")
                p.Db.Project.description
            , sprintf
                (if snd model.cursor = Tags && idx = fst model.cursor
                 then ">%s<"
                 else "%s")
                (String.concat ", " p.Db.Project.tags) ))
        in
        c1 := max !c1 (String.length a);
        c2 := max !c2 (String.length b);
        c3 := max !c3 (String.length c);
        res)
      model.projects
  in
  List.fold_left
    (fun acc (a, b, c) ->
      acc ^ Format.sprintf "%-*s | %-*s | %-*s\n" !c1 a !c2 b !c3 c)
    (Format.sprintf
       "%*s | %*s | %*s\n"
       !c1
       "Project"
       !c2
       "Description"
       !c3
       "Tags")
    table
;;

let app = Minttea.app ~initial_model ~init ~update ~view ()
let start () = Minttea.start app

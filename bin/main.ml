open Cmdliner

let sql_uri = "sqlite3:proj.sqlite"

(** [run_with_db f] runs [f] with a database connection using [Lwt_main.run]. *)
let run_with_db f =
  let f' =
    let open Lwt_result.Syntax in
    let* conn = Caqti_lwt_unix.connect @@ Uri.of_string sql_uri in
    let* () = Petrol.StaticSchema.initialise Db.Schema.schema conn in
    let* () = f conn in
    Lwt.return_ok ()
  in
  let _ = Lwt_main.run f' in
  ()
;;

(* Define functions for commands *)
let ls () =
  let fs_projects =
    List.filter (fun x ->
      Sys.is_directory x
      && ((* Remove hidden directories from list *) not
          @@ String.starts_with ~prefix:"." x))
    @@ Array.to_list
    @@ Sys.readdir "."
  in
  run_with_db (fun db ->
    let open Lwt.Syntax in
    let* projects = Db.Project.list db in
    let* projects = Caqti_lwt.or_fail projects in
    let project_names = List.map (fun p -> p.Db.Project.dirname) projects in
    let* () =
      (* Sequentially insert projects that are not in the database *)
      Lwt_list.iter_s
        (fun p ->
          if not @@ List.mem p project_names
          then
            let* res = Db.Project.insert ~dirname:p db in
            Caqti_lwt.or_fail res
          else Lwt.return ())
        fs_projects
    in
    let* projects = Db.Project.list db in
    let* projects = Caqti_lwt.or_fail projects in
    let c1 = ref 7 in
    let c2 = ref 11 in
    let c3 = ref 4 in
    let table =
      List.map
        (fun p ->
          let ((a, b, c) as res) =
            Format.(
              ( sprintf "%s" p.Db.Project.dirname
              , sprintf "%s" p.Db.Project.description
              , sprintf "%s" (String.concat ", " p.Db.Project.tags) ))
          in
          c1 := max !c1 (String.length a);
          c2 := max !c2 (String.length b);
          c3 := max !c3 (String.length c);
          res)
        projects
    in
    Format.printf "%*s | %*s | %*s\n" !c1 "Project" !c2 "Description" !c3 "Tags";
    List.iter
      (fun (a, b, c) -> Format.printf "%-*s | %-*s | %-*s\n" !c1 a !c2 b !c3 c)
      table;
    Lwt.return_ok ())
;;

let edit name desc tags =
  run_with_db (fun db ->
    if not @@ Sys.file_exists name
    then
      (* TODO: Better error. This feels hacky *)
      raise @@ Invalid_argument "Project does not exist"
    else Db.Project.update ~dirname:name ?description:desc ?tags db)
;;

let init () = print_endline "This will initialize the projects directory"

(* Define commands and terms *)
let ls_t = Term.(const ls $ const ())
let ls_cmd = Cmd.v (Cmd.info "list") ls_t

let edit_t =
  Term.(
    const edit
    $ Arg.(required & pos 0 (some string) None & info [] ~docv:"DIRNAME")
    $ Arg.(
        value
        & opt (some string) None
        & info [ "d"; "description" ] ~docv:"DESC")
    $ Arg.(
        value
        & opt (some @@ list string) None
        & info [ "t"; "tags" ] ~docv:"TAGS"))
;;

let edit_cmd = Cmd.v (Cmd.info "edit") edit_t
let init_t = Term.(const init $ const ())
let init_cmd = Cmd.v (Cmd.info "init") init_t
let tui_t = Term.(const Ui.start $ const ())
let tui_cmd = Cmd.v (Cmd.info "tui") tui_t

(* Define the command group *)
let cmd =
  Cmd.group
    ?default:(Some ls_t)
    (Cmd.info "p")
    [ ls_cmd; edit_cmd; init_cmd; tui_cmd ]
;;

let () = exit @@ Cmd.eval cmd

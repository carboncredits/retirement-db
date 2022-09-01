open Retirement

let await = Lwt_eio.Promise.await_lwt

module Schema =
  Irmin_git.Schema.Make (Git_unix.Store) (Data)
    (Irmin_git.Branch.Make (Irmin.Branch.String))

module I = struct
  module Schema = Schema
  include Irmin_git_unix.FS.Make (Schema)
end

module Store = Store.Make (I)

module Remote = struct
  let remote = None
end

module Custom_types = struct
  include Irmin_graphql.Server.Default_types (Store.I)
  module Contents = Data
end

module Server =
  Irmin_graphql_unix.Server.Make_ext (Store.I) (Remote) (Custom_types)

let config root = Irmin_git.config ~bare:true root

let fetch db remote =
  let r = I.remote remote |> await in
  match Store.Sync.pull db r `Set |> await with
  | Ok status ->
      Logs.info (fun f ->
          f "Fetch %s with status %a" remote Store.Sync.pp_status status)
  | Error e ->
      Logs.err (fun f -> f "Failed to fetch with %a" Store.Sync.pp_pull_error e)

let callback schema _conn req body =
  let open Cohttp_lwt in
  let open Lwt.Syntax in
  match
    ( Request.meth req,
      Astring.String.cuts ~empty:false ~sep:"/" (Request.resource req) )
  with
  | `POST, [ "graphql" ] -> (
      let+ res = Server.execute_request schema req body in
      match res with
      | `Response (r, b) ->
          (* TODO: The `*` option is for development purposes only *)
          let headers =
            Cohttp.Header.add r.headers "Access-Control-Allow-Origin" "*"
          in
          `Response ({ r with headers }, b)
      | v -> v)
  | `OPTIONS, [ "graphql" ] ->
      Logs.info (fun f -> f "Answering options request for graphql endpoint");
      let headers =
        Cohttp.Header.of_list
          [
            ("Access-Control-Allow-Origin", "*");
            ("Access-Control-Allow-Methods", "POST");
            ("Access-Control-Allow-Methods", "GET");
            ("Access-Control-Max-Age", "86400");
            ("Access-Control-Allow-Headers", "Content-Type");
          ]
      in
      let+ r =
        Cohttp_lwt_unix.Server.respond ~headers ~status:`No_content
          ~body:Cohttp_lwt.Body.empty ()
      in
      `Response r
  | _meth, s ->
      let* b = Cohttp_lwt.Body.to_string body in
      Logs.info (fun f ->
          f "Not found %a %a %s" Fmt.(list string) s Request.pp_hum req b);
      let+ r =
        Cohttp_lwt_unix.Server.respond ~status:`Not_found
          ~body:Cohttp_lwt.Body.empty ()
      in
      `Response r

let server dir remote =
  let config = config dir in
  let s = Store.repository config in
  (match remote with
  | Some remote -> fetch (Store.of_branch s) remote
  | _ -> ());
  let schema = Server.schema s in
  Cohttp_lwt_unix.Server.make_response_action ~callback:(callback schema) ()

open Cmdliner

let logs =
  let init style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs.format_reporter ())
  in
  let open Cmdliner in
  let docs = Manpage.s_common_options in
  Term.(const init $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let remote =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The remote to fetch from" ~docv:"REMOTE" [ "remote" ]

let directory =
  Arg.value
  @@ Arg.opt Arg.string "/tmp/projects"
  @@ Arg.info ~doc:"The directory on the filesystem to use for the repository"
       ~docv:"DIRECTORY" [ "directory" ]

let port =
  Arg.value @@ Arg.opt Arg.int 8080
  @@ Arg.info ~doc:"The port to run the server on" ~docv:"PORT" [ "port" ]

let serve' ?remote ~dir ~src:_ ~port () =
  let server = server dir remote in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let on_exn exn = Logs.err (fun f -> f "Error: %a" Fmt.exn exn) in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  Logs.info (fun f -> f "Server running at http://localhost:%i" port);
  Cohttp_lwt_unix.Server.create ~on_exn ~ctx ~mode:(`TCP (`Port port)) server

let serve _fs =
  let serve () remote dir src port =
    await @@ serve' ?remote ~dir ~src ~port ()
  in
  let doc = "Serve a project repository over a GraphQL interface" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info
  @@ Term.(const serve $ logs $ remote $ directory $ const "::" $ port)

let cmds env =
  let fs = Eio.Stdenv.fs env in
  [ serve fs ]

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _token ->
  let doc = "an irmin-graphql server for project configurations" in
  let info = Cmd.info "irmin-project-db" ~doc ~version in
  exit (Cmd.eval @@ Cmd.group info (cmds env))

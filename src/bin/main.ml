open Retirement
open Eio
module Md = Multihash_digestif

module I = struct
  include Irmin_fs_unix.Make (Retirement.Schema)
end

module Store = Store.Make (I)

module Remote = struct
  let remote = None
end

let config root = Irmin_fs.config root

let json_headers s =
  Http.Header.of_list
    [
      ("Content-Type", "application/json");
      ("Content-Length", string_of_int @@ String.length s);
    ]

let current_api_version = "v1"

let response_with_body body =
  ( Http.Response.make ~headers:(json_headers body) ~status:`OK (),
    Cohttp_eio.Body.Fixed body )

let send_error err =
  let resp =
    Retirement_data.Json.string_of_string_response
      { errors = [ err ]; data = "Failure" }
  in
  response_with_body resp

module Rest = Retirement.Data.Rest

let read_body req body f =
  let data =
    Cohttp_eio.Server.read_fixed req body
    |> Option.to_result ~none:(`Msg "No body!")
  in
  try Result.map f data
  with _ -> Error (`Msg "Failed to parse the request arguments")

let v1_callback ~clock repo main ((req, body, _) : Cohttp_eio.Server.request) =
  let open Http in
  let open Cohttp_eio in
  match
    ( Request.meth req,
      Astring.String.cuts ~empty:false ~sep:"/" (Request.resource req) )
  with
  | `POST, [ "add" ] -> (
      match read_body req body Retirement_data.Json.set_request_of_string with
      | Error (`Msg m) -> send_error m
      | Ok data -> (
          match Store.add_project ~clock main data.path data.value with
          | Error e ->
              let body =
                Retirement_data.Json.string_of_string_response
                  { errors = [ Store.add_error_to_string e ]; data = "Failure" }
              in
              response_with_body body
          | Ok None ->
              let body =
                Retirement_data.Json.string_of_string_response
                  { errors = [ "No commit was made" ]; data = "Failure" }
              in
              response_with_body body
          | Ok (Some commit) ->
              let body =
                Rest.Response.set_to_json { errors = []; data = commit }
              in
              response_with_body body))
  | `POST, [ "get"; "hash" ] -> (
      match
        read_body req body Retirement_data.Json.get_hash_request_of_string
      with
      | Error (`Msg m) -> send_error m
      | Ok data -> (
          match Store.of_commit repo data.commit with
          | Error (`Msg m) -> send_error m
          | Ok store ->
              let project = Store.get_project store data.path in
              let hash =
                I.Contents.hash project |> Irmin.Type.to_string I.Hash.t
              in
              let response =
                Rest.Response.get_hash_to_json { errors = []; data = hash }
              in
              response_with_body response))
  | `POST, [ "get"; "content" ] -> (
      match
        read_body req body Retirement_data.Json.get_content_request_of_string
      with
      | Error (`Msg m) -> send_error m
      | Ok data -> (
          match Store.get_project_by_hash repo data.hash with
          | Error (`Msg m) -> send_error m
          | Ok None -> send_error ("No project found at hash " ^ data.hash)
          | Ok (Some project) ->
              let response =
                Rest.Response.get_content_to_json
                  { errors = []; data = project }
              in
              response_with_body response))
  | `GET, [ "json"; year; month ] ->
      let items =
        Store.get_all main [ year; month ] |> Data.list_to_json_string
      in
      ( Http.Response.make ~headers:(json_headers items) ~status:`OK (),
        Body.Fixed items )
  | _meth, s ->
      Logs.info (fun f ->
          f "Not found %a %a" Fmt.(list string) s Request.pp req);
      Server.not_found_response

let server ~clock dir =
  let config = config dir in
  let repo = Store.repository config in
  let main = Store.of_branch repo in
  v1_callback ~clock repo main

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

let directory =
  Arg.value
  @@ Arg.opt Arg.string "/tmp/projects"
  @@ Arg.info ~doc:"The directory on the filesystem to use for the repository"
       ~docv:"DIRECTORY" [ "directory" ]

let path =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The / segmented path for the value to be stored at"
       ~docv:"PATH" [ "path" ]

let port =
  Arg.value @@ Arg.opt Arg.int 8080
  @@ Arg.info ~doc:"The port to run the server on" ~docv:"PORT" [ "port" ]

let run_domain ssock handler =
  let on_error exn =
    Printf.fprintf stderr "Error handling connection: %s\n%!"
      (Printexc.to_string exn)
  in
  let handler = Cohttp_eio.Server.connection_handler handler in
  Switch.run (fun sw ->
      let rec loop () =
        Eio.Net.accept_fork ~sw ssock ~on_error handler;
        loop ()
      in
      loop ())

let run ?(socket_backlog = 128) ?(domains = 2) ~port env handler =
  Switch.run @@ fun sw ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let ssock =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~reuse_port:true
      ~backlog:socket_backlog
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  for _ = 2 to domains do
    Eio.Std.Fiber.fork ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () -> run_domain ssock handler))
  done;
  run_domain ssock handler

let serve' ~env ~dir ~src:_ ~port () =
  Irmin_fs.run env#fs @@ fun () ->
  let server = server ~clock:env#clock dir in
  Logs.info (fun f -> f "Server running at http://localhost:%i" port);
  run ~port env server

let serve env =
  let serve () dir src port = serve' ~env ~dir ~src ~port () in
  let doc = "Serve a project repository over an HTTP interface" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info @@ Term.(const serve $ logs $ directory $ const "::" $ port)

let add_project ~fs ~clock stdin =
  let add () dir path =
    Irmin_fs.run fs @@ fun () ->
    let repo = Store.repository (config dir) in
    let main = Store.of_branch repo in
    let buf = Buffer.create 1028 in
    let () = Eio.Flow.(copy stdin (buffer_sink buf)) in
    let segs = String.split_on_char '/' path in
    match Store.add_project_json ~clock main segs (Buffer.contents buf) with
    | Ok _hash -> ()
    | Error _ -> Fmt.epr "Failed to store!"
  in
  let doc = "Add a project using JSON read from stdin" in
  let info = Cmd.info "add" ~doc in
  Cmd.v info @@ Term.(const add $ logs $ directory $ path)

let dummy_details =
  Retirement_data.Types.
    {
      flight_details = [];
      train_details = [];
      taxi_details = [];
      additional_details = [];
      primary_reason = `Conference;
      secondary_reason = None;
      reason_text = "Some reason for travelling!";
    }

let dummy stdout =
  let dummy () =
    Eio.Flow.(copy_string (Data.to_pretty_string Data.dummy_details) stdout)
  in
  let doc = "Write a dummy retirement JSON blob to stdout" in
  let info = Cmd.info "dummy" ~doc in
  Cmd.v info @@ Term.(const dummy $ logs)

let cmds env =
  [
    serve env;
    add_project ~fs:env#fs ~clock:env#clock env#stdin;
    dummy env#stdout;
  ]

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let () =
  Eio_main.run @@ fun env ->
  let doc = "an irmin http server for retirement data" in
  let info = Cmd.info "retirement-db" ~doc ~version in
  exit (Cmd.eval @@ Cmd.group info (cmds env))

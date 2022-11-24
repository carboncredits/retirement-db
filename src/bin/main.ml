open Retirement
module Md = Multihash_digestif

let await = Lwt_eio.Promise.await_lwt

module I = struct
  include Irmin_fs_unix.Make (Retirement.Schema)
end

module Store = Store.Make (I)

module Remote = struct
  let remote = None
end

module Custom_types = struct
  include Irmin_graphql.Server.Default_types (Store.I)
  module Contents = Data
end

let config root = Irmin_fs.config root

let json_headers =
  Cohttp.Header.of_list [ ("Content-Type", "application/json") ]

let current_api_version = "v1"

let response_with_body body =
  let open Lwt.Syntax in
  let+ r =
    Cohttp_lwt_unix.Server.respond_string ~headers:json_headers ~status:`OK
      ~body ()
  in
  `Response r

let send_error err =
  let resp =
    Retirement_data.Json.string_of_response Buffer.add_string
      { errors = [ err ]; data = "Failure" }
  in
  response_with_body resp

module Rest = Retirement.Data.Rest

let v1_callback repo main _conn req body =
  let open Cohttp_lwt in
  let open Lwt.Syntax in
  match
    ( Request.meth req,
      Astring.String.cuts ~empty:false ~sep:"/" (Request.resource req) )
  with
  | `POST, [ "add" ] -> (
      let* data = Cohttp_lwt.Body.to_string body in
      let data =
        try Ok (Retirement_data.Json.set_request_of_string data)
        with _ -> Error (`Msg "Failed to parse the request arguments")
      in
      match data with
      | Error (`Msg m) -> send_error m
      | Ok data -> (
          match Store.add_project main data.path data.value with
          | Error e ->
              let body =
                Retirement_data.Json.string_of_response Buffer.add_string
                  { errors = [ Store.add_error_to_string e ]; data = "Failure" }
              in
              response_with_body body
          | Ok None ->
              let body =
                Retirement_data.Json.string_of_response Buffer.add_string
                  { errors = [ "No commit was made" ]; data = "Failure" }
              in
              response_with_body body
          | Ok (Some commit) ->
              let body =
                Rest.Response.set_to_json { errors = []; data = commit }
              in
              response_with_body body))
  | `POST, [ "get"; "hash" ] -> (
      let* data = Cohttp_lwt.Body.to_string body in
      let data =
        try Ok (Retirement_data.Json.get_hash_request_of_string data)
        with _ -> Error (`Msg "Failed to parse the request arguments")
      in
      match data with
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
      let* data = Cohttp_lwt.Body.to_string body in
      let data =
        try Ok (Retirement_data.Json.get_content_request_of_string data)
        with _ -> Error (`Msg "Failed to parse the request arguments")
      in
      match data with
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
      let+ r =
        Cohttp_lwt_unix.Server.respond_string ~headers:json_headers ~status:`OK
          ~body:items ()
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

let server dir =
  let config = config dir in
  let repo = Store.repository config in
  let main = Store.of_branch repo in
  Cohttp_lwt_unix.Server.make_response_action ~callback:(v1_callback repo main)
    ()

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

let serve' ~dir ~src:_ ~port () =
  let server = server dir in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let on_exn exn = Logs.err (fun f -> f "Error: %a" Fmt.exn exn) in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  Logs.info (fun f -> f "Server running at http://localhost:%i" port);
  Cohttp_lwt_unix.Server.create ~on_exn ~ctx ~mode:(`TCP (`Port port)) server

let serve _fs =
  let serve () dir src port = await @@ serve' ~dir ~src ~port () in
  let doc = "Serve a project repository over a GraphQL interface" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info @@ Term.(const serve $ logs $ directory $ const "::" $ port)

let add_project stdin =
  let add () dir path =
    let repo = Store.repository (config dir) in
    let main = Store.of_branch repo in
    let buf = Buffer.create 1028 in
    let () = Eio.Flow.(copy stdin (buffer_sink buf)) in
    let segs = String.split_on_char '/' path in
    match Store.add_project_json main segs (Buffer.contents buf) with
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
  let fs = Eio.Stdenv.fs env in
  [ serve fs; add_project env#stdin; dummy env#stdout ]

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

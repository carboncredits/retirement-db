open Lwt.Syntax
open Retirement
module Md = Multihash_digestif
module Server = Cohttp_lwt_unix.Server

module Ifs = struct
  include Irmin_fs_unix.Make (Retirement.Schema)
end

module Store = Store.Make (Ifs)

let config root = Irmin_fs.config root

let json_headers s =
  Cohttp.Header.of_list
    [
      ("Content-Type", "application/json");
      ("Content-Length", string_of_int @@ String.length s);
    ]

let csv_headers s =
  Cohttp.Header.of_list
    [
      ("Content-Type", "text/csv");
      ("Content-Length", string_of_int @@ String.length s);
    ]

let _current_api_version = "v1"

let success ppf s =
  Fmt.pf ppf "[%a]: %s" Fmt.(styled (`Fg `Green) string) "SUCCESS" s

let failure ppf s =
  Fmt.pf ppf "[%a]: %s" Fmt.(styled (`Fg `Red) string) "FAILURE" s

let response_with_body body =
  ( Cohttp.Response.make ~headers:(json_headers body) ~status:`OK (),
    Cohttp_lwt.Body.of_string body )

let send_error err =
  let resp =
    Retirement_data.Json.string_of_string_response
      { errors = [ err ]; data = "Failure" }
  in
  Lwt.return (response_with_body resp)

module Rest = Retirement.Data.Rest

let read_body body f =
  let+ data = Cohttp_lwt.Body.to_string body in
  try Ok (f data)
  with _ -> Error (`Msg "Failed to parse the request arguments")

let v1_callback ~clock store (req : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  let open Cohttp in
  match
    ( Request.meth req,
      Astring.String.cuts ~empty:false ~sep:"/" (Request.resource req) )
  with
  | `POST, [ "tx"; "begin" ] -> (
      let* body =
        read_body body Retirement_data.Json.begin_tx_request_of_string
      in
      match body with
      | Error (`Msg m) -> send_error m
      | Ok data -> (
          let+ tx = Store.begin_transaction ~clock store data.value in
          match tx with
          | Error e ->
              let body =
                Retirement_data.Json.string_of_string_response
                  { errors = [ Store.tx_error_to_string e ]; data = "Failure" }
              in
              response_with_body body
          | Ok content_hash ->
              let body =
                Rest.Response.begin_tx_to_json
                  { errors = []; data = content_hash }
              in
              response_with_body body))
  | `POST, [ "tx"; "complete" ] -> (
      let* body =
        read_body body Retirement_data.Json.complete_tx_request_of_string
      in
      match body with
      | Error (`Msg m) -> send_error m
      | Ok data -> (
          let+ tx =
            Store.complete_transaction ~clock store ~hash:data.hash
              ~tx:data.tx_id
          in
          match tx with
          | Error e ->
              let err =
                match e with `Msg m -> m | `Not_pending -> "Not pending"
              in
              let body =
                Retirement_data.Json.string_of_string_response
                  { errors = [ err ]; data = "Failure" }
              in
              response_with_body body
          | Ok content_hash ->
              let body =
                Rest.Response.begin_tx_to_json
                  { errors = []; data = content_hash }
              in
              response_with_body body))
  | `POST, [ "tx"; "status" ] -> (
      let* body =
        read_body body Retirement_data.Json.check_tx_status_request_of_string
      in
      match body with
      | Error (`Msg m) -> send_error m
      | Ok data ->
          let* status =
            let+ id = Store.get_transaction_id store ~hash:data.hash in
            match id with
            | Some (Some c) -> `Complete c
            | Some None -> `Pending
            | None -> `Not_started
          in
          let body =
            Rest.Response.check_tx_status_to_json { errors = []; data = status }
          in
          Lwt.return (response_with_body body))
  | `POST, [ "get"; "content" ] -> (
      let* body =
        read_body body Retirement_data.Json.get_content_request_of_string
      in
      match body with
      | Error (`Msg m) -> send_error m
      | Ok data -> (
          let* find = Store.find store ~hash:data.hash in
          match find with
          | None -> send_error ("No project found at hash " ^ data.hash)
          | Some contents ->
              let response =
                Rest.Response.get_content_to_json
                  { errors = []; data = contents }
              in
              Lwt.return @@ response_with_body response))
  | `POST, [ "get"; "bookers" ] -> (
      let* body =
        read_body body Retirement_data.Json.get_bookers_request_of_string
      in
      match body with
      | Error (`Msg m) -> send_error m
      | Ok data ->
          if data.months < 1 then send_error "Months should be greater than 0!"
          else
            let current_year, current_month, _ =
              Ptime.of_float_s (Unix.gettimeofday ())
              |> Option.get |> Ptime.to_date
            in
            let* contents =
              Store.lookup_bookers_transacted store ~booker:data.booker
                ~months:data.months ~current_year ~current_month
            in
            let data =
              List.map
                (fun v -> (v, Store.hash_content { v with tx_id = None }))
                contents
            in
            let response =
              Rest.Response.get_bookers_to_json { errors = []; data }
            in
            Lwt.return @@ response_with_body response)
  | `GET, [ "json"; year; month ] ->
      let+ items =
        Store.lookup_all_transacted store [ year; month ]
        |> Lwt.map Data.list_to_json_string
      in
      ( Cohttp.Response.make ~headers:(json_headers items) ~status:`OK (),
        Cohttp_lwt.Body.of_string items )
  | `GET, [ "csv"; "flights"; year; month ] ->
      let+ body =
        Store.csv_by_flight ~year:(int_of_string year)
          ~month:(int_of_string month) store
      in
      ( Cohttp.Response.make ~headers:(csv_headers body) ~status:`OK (),
        Cohttp_lwt.Body.of_string body )
  | `GET, [ "csv"; "finance"; year; month ] ->
      let+ body =
        Store.csv_by_finance ~year:(int_of_string year)
          ~month:(int_of_string month) store
      in
      ( Cohttp.Response.make ~headers:(csv_headers body) ~status:`OK (),
        Cohttp_lwt.Body.of_string body )
  | _meth, s ->
      Logs.info (fun f ->
          f "Not found %a %a" Fmt.(list string) s Cohttp.Request.pp_hum req);
      Server.respond_not_found ()

let init_store dir = Store.v (config dir)

let server ~clock dir req body =
  let* store = init_store dir in
  v1_callback ~clock store req body

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

let port =
  Arg.value @@ Arg.opt Arg.int 8080
  @@ Arg.info ~doc:"The port to run the server on" ~docv:"PORT" [ "port" ]

let content_address =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The hash of the content" ~docv:"HASH" [ "hash" ]

let timestamp =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The RFC3339 timestamp to use for a piece of content"
       ~docv:"TIMESTAMP" [ "timestamp" ]

let run ?(socket_backlog = 128) ~port callback =
  let callback _conn = callback in
  Cohttp_lwt_unix.Server.create ~backlog:socket_backlog
    ~mode:(`TCP (`Port port))
    (Server.make ~callback ())

let serve' ~clock ~dir ~port () =
  let server = server ~clock dir in
  Logs.info (fun f -> f "Server running at http://localhost:%i" port);
  run ~port server

let unix_clock =
  object
    method now = Unix.gettimeofday ()
  end

open Cmdliner

let serve =
  let serve () dir port =
    Lwt_main.run @@ serve' ~clock:unix_clock ~dir ~port ()
  in
  let doc = "Serve a project repository over an HTTP interface" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info @@ Term.(const serve $ logs $ directory $ port)

let begin_tx ~clock =
  let add () dir =
    Lwt_main.run
    @@ let* store = init_store dir in
       let data = In_channel.input_all In_channel.stdin in
       let contents =
         match Data.of_string data with
         | Ok v -> v
         | Error (`Msg s) -> failwith s
       in
       let+ tx = Store.begin_transaction ~clock store contents in
       match tx with
       | Ok hash -> Fmt.pr "%s" hash
       | Error _ -> Fmt.epr "Failed to store!"
  in
  let doc = "Begin a transaction for a blob of data" in
  let info = Cmd.info "begin-tx" ~doc in
  Cmd.v info @@ Term.(const add $ logs $ directory)

let complete_tx ~clock =
  let complete () dir hash =
    Lwt_main.run
    @@ let* store = init_store dir in
       let data = In_channel.input_all In_channel.stdin in
       let+ tx = Store.complete_transaction ~clock store ~hash ~tx:data in
       match tx with
       | Ok hash -> Fmt.pr "%a" success hash
       | Error (`Msg m) -> Fmt.epr "%a" failure m
       | Error `Not_pending -> Fmt.epr "%a" failure "Value is not pending!"
  in
  let doc =
    "Complete a transaction with a transaction ID from stdin, which will fail \
     if the blob is not already pending by beginning the transaction."
  in
  let info = Cmd.info "complete-tx" ~doc in
  Cmd.v info @@ Term.(const complete $ logs $ directory $ content_address)

let pp_status ppf = function
  | Some (Some c) ->
      Fmt.pf ppf "%a: %s" Fmt.(styled (`Fg `Green) string) "COMPLETE" c
  | Some None -> Fmt.pf ppf "%a" Fmt.(styled (`Fg `Yellow) string) "PENDING"
  | None -> Fmt.pf ppf "%a" Fmt.(styled (`Fg `Red) string) "NO TX FOUND"

let check_tx =
  let check () dir =
    Lwt_main.run
    @@ let* store = init_store dir in
       let data = In_channel.input_all In_channel.stdin in
       let+ status = Store.get_transaction_id store ~hash:data in
       pp_status Fmt.stdout status
  in
  let doc = "For a particular hash, prints the status of the transaction." in
  let info = Cmd.info "check-tx" ~doc in
  Cmd.v info @@ Term.(const check $ logs $ directory)

let dummy clock =
  let dummy () some_timestamp =
    Lwt_main.run @@ Lwt.return
    @@ Out_channel.output_string Out_channel.stdout
         (Data.to_pretty_string
            Data.(dummy_details ?timestamp:some_timestamp clock))
  in
  let doc = "Write a dummy retirement JSON blob to stdout" in
  let info = Cmd.info "dummy" ~doc in
  Cmd.v info @@ Term.(const dummy $ logs $ timestamp)

let lookup =
  let check () dir hash =
    Lwt_main.run
    @@ let* store = init_store dir in
       let+ contents = Store.find store ~hash in
       Fmt.pr "%a%!" (Fmt.option @@ Irmin.Type.pp Data.t) contents
  in
  let doc = "Lookup data using hash." in
  let info = Cmd.info "lookup" ~doc in
  Cmd.v info @@ Term.(const check $ logs $ directory $ content_address)

let dump =
  let check () dir =
    Lwt_main.run
    @@ let* store = init_store dir in
       Store.Private.dump Fmt.stdout store
  in
  let doc = "Dump all of the contents of the store to stdout." in
  let info = Cmd.info "dump" ~doc in
  Cmd.v info @@ Term.(const check $ logs $ directory)

let cmds clock =
  [
    serve;
    begin_tx ~clock;
    complete_tx ~clock;
    check_tx;
    dummy clock;
    lookup;
    dump;
  ]

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let () =
  let doc = "an irmin http server for retirement data" in
  let info = Cmd.info "retirement" ~doc ~version in
  exit (Cmd.eval @@ Cmd.group info (cmds unix_clock))

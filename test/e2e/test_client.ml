open Lwt.Syntax
module Client = Cohttp_lwt_unix.Client
module Md = Multihash_digestif
module Store = Irmin_mem.Make (Retirement.Schema)

let server_port = "9191"
let headers = Cohttp.Header.of_list [ ("Content-Type", "application/json") ]

let get_json ?headers http uri =
  (* let host  =
       match Uri.host uri with
       | None -> Fmt.failwith "Missing host in URL %a" Uri.pp uri
       | Some host -> (
           match (Unix.gethostbyname host).h_addr_list with
           | [||] -> Fmt.failwith "Unknown host %S in URL %a" host Uri.pp uri
           | arr -> (host, arr.(0))
           | exception Not_found ->
               Fmt.failwith "Unknown host %S in URL %a" host Uri.pp uri)
     in
     let port = Uri.port uri |> Option.value ~default:8080 in *)
  let* (_resp, body) : Cohttp.Response.t * Cohttp_lwt.Body.t =
    http ?headers uri
  in
  Cohttp_lwt.Body.to_string body

let req_to_json response ~host ~path body =
  let open Cohttp_lwt_unix in
  let http ?headers host =
    Client.post
      ~body:(Cohttp_lwt.Body.of_string body)
      ?headers (Uri.with_path host path)
  in
  let+ body = get_json ~headers http host in
  let json = Yojson.Basic.from_string body in
  match Yojson.Basic.Util.member "errors" json |> Yojson.Basic.Util.to_list with
  | [] -> Ok (response body)
  | ls -> Error (List.map Yojson.Basic.Util.to_string ls)

let store_content =
  Alcotest.of_pp (fun ppf v ->
      Fmt.pf ppf "%s" (Retirement_data.Json.string_of_t v))

let status =
  Alcotest.of_pp (fun ppf v ->
      Fmt.pf ppf "%s" (Retirement_data.Json.string_of_tx_status v))

(* let multihash = Alcotest.testable Md.pp Md.equal *)

module Rest = Retirement.Data.Rest

let set_and_get_hash ~clock host () =
  let reason = "Test number 1" in
  let timestamp = Retirement.Data.current_ts clock in
  let value =
    Retirement.Data.v
      ~version:{ major = 0; minor = 1; patch = None }
      ~timestamp "xyz123"
      { crsid = "abc123"; department = "CST"; name = "Alice" }
      (`Grant
        {
          sponsor_and_pi_confirmation = true;
          award = "award";
          project = "project";
          task = "task";
        })
      {
        flight_details = [];
        train_details = [];
        taxi_details = [];
        additional_details = [];
        primary_reason = `Conference;
        secondary_reason = None;
        reason_text = reason;
      }
      {
        token_id = 1234;
        project_name = "Gola";
        minter = "abcd1234wxyz5678";
        kyc = "1234abcd5678wxyz";
        amount = 556789;
      }
  in
  let begin_value =
    Retirement_data.Types.{ value } |> Rest.Request.begin_tx_to_json
  in
  let* content =
    req_to_json Rest.Response.begin_tx_of_json ~host ~path:"/tx/begin"
      begin_value
    |> Lwt.map Result.get_ok
  in
  let hash = content.data in
  let mh = Store.Contents.hash value in
  Alcotest.(check string)
    "same hash"
    (Irmin.Type.to_string Store.Hash.t mh)
    hash;
  let get_content_value =
    Retirement_data.Types.{ hash } |> Rest.Request.get_content_to_json
  in
  let* stored_value =
    req_to_json Rest.Response.get_content_of_json ~host ~path:"/get/content"
      get_content_value
    |> Lwt.map Result.get_ok
  in
  let v' = stored_value.data in
  Alcotest.(check store_content) "same stored content" value v';
  let check_value =
    Retirement_data.Types.{ hash } |> Rest.Request.check_tx_status_to_json
  in
  let* check =
    req_to_json Rest.Response.check_tx_status_of_json ~host ~path:"/tx/status"
      check_value
    |> Lwt.map Result.get_ok
  in
  Alcotest.(check status) "same tx status" `Pending check.data;
  let complete_value =
    Retirement_data.Types.{ hash; tx_id = "ABCDEFGH" }
    |> Rest.Request.complete_tx_to_json
  in
  let* _check =
    req_to_json Rest.Response.complete_tx_of_json ~host ~path:"/tx/complete"
      complete_value
  in
  let check_value =
    Retirement_data.Types.{ hash } |> Rest.Request.check_tx_status_to_json
  in
  let* check =
    req_to_json Rest.Response.check_tx_status_of_json ~host ~path:"/tx/status"
      check_value
    |> Lwt.map Result.get_ok
  in
  Alcotest.(check bool)
    "same tx status" true
    (match check.data with `Complete _ -> true | _ -> false);
  let bookers_value, bad_bookers_value =
    let b = Retirement_data.Types.{ booker = "xyz123"; months = 3 } in
    Rest.Request.
      (get_bookers_to_json b, get_bookers_to_json { b with months = 0 })
  in
  let* check, errors =
    let* a =
      req_to_json Rest.Response.get_bookers_of_json ~host ~path:"/get/bookers"
        bookers_value
      |> Lwt.map Result.get_ok
    in
    let+ b =
      req_to_json Rest.Response.get_bookers_of_json ~host ~path:"/get/bookers"
        bad_bookers_value
      |> Lwt.map Result.get_error
    in
    (a, b)
  in
  Alcotest.(check (list store_content))
    "same bookers bookings"
    [ { value with tx_id = Some "ABCDEFGH" } ]
    (List.map fst check.data);
  Alcotest.(check int) "same errors" 1 (List.length errors);
  (* This should fail because we've already added the value *)
  let begin_value =
    Retirement_data.Types.{ value } |> Rest.Request.begin_tx_to_json
  in
  let+ errors =
    req_to_json Rest.Response.begin_tx_of_json ~host ~path:"/tx/begin"
      begin_value
    |> Lwt.map Result.get_error
  in
  Alcotest.(check int) "same errors" 1 (List.length errors)

let client clock () =
  let uri =
    Uri.of_string
    @@
    try Sys.getenv "SERVER_HOST"
    with Not_found -> "http://localhost:" ^ server_port
  in
  Alcotest.run ~and_exit:false "retirement-db-e2e"
    [
      ( "basic",
        [
          Alcotest.test_case "get-and-set" `Quick (fun () ->
              Lwt_main.run @@ set_and_get_hash ~clock uri ());
        ] );
    ]

let with_process (v, argv) f =
  let pid = Lwt_process.open_process (v, argv) in
  Fun.protect ~finally:(fun () -> pid#kill Sys.sigkill) f

let unix_clock =
  object
    method now = Unix.gettimeofday ()
  end

let () =
  Random.self_init ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  let run f =
    with_process
      ( "retirement",
        [|
          "retirement";
          "serve";
          "--directory";
          "./var";
          "--port";
          server_port
          (* "--verbosity"; *)
          (* "debug"; *);
        |] )
      (fun () ->
        Unix.sleepf 2.;
        f ())
  in
  run (client unix_clock)

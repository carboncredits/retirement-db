module Client = Cohttp_lwt_unix.Client
module Md = Multihash_digestif
module Store = Irmin_mem.Make (Retirement.Schema)

let await = Lwt_eio.Promise.await_lwt
let headers = Cohttp.Header.of_list [ ("Content-Type", "application/json") ]
let ( / ) t s = Yojson.Safe.Util.member s t

let req_to_json response uri body =
  let _resp, body = await @@ Client.post ~body:(`String body) ~headers uri in
  let body = await @@ Cohttp_lwt.Body.to_string body in
  response body

let store_content =
  Alcotest.of_pp (fun ppf v ->
      Fmt.pf ppf "%s" (Retirement_data.Json.string_of_t v))

(* let multihash = Alcotest.testable Md.pp Md.equal *)

module Rest = Retirement.Data.Rest

let set_and_get_hash uri () =
  let reason = "Test number 1" in
  let value =
    Retirement.Data.v
      ~version:{ major = 0; minor = 1; patch = None }
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
  let string_value =
    Retirement_data.Types.{ path = [ "hello" ]; value }
    |> Rest.Request.set_to_json
  in
  let content =
    req_to_json Rest.Response.set_of_json (Uri.with_path uri "/add")
      string_value
  in
  let commit = content.data in
  let get_hash_value =
    Retirement_data.Types.{ path = [ "hello" ]; commit }
    |> Rest.Request.get_hash_to_json
  in
  let store_value =
    req_to_json Rest.Response.get_hash_of_json
      (Uri.with_path uri "/get/hash")
      get_hash_value
  in
  let hash = store_value.data in
  let mh = Store.Contents.hash value in
  Alcotest.(check string)
    "same hash"
    (Irmin.Type.to_string Store.Hash.t mh)
    hash;
  let get_content_value =
    Retirement_data.Types.{ hash } |> Rest.Request.get_content_to_json
  in
  let stored_value =
    req_to_json Rest.Response.get_content_of_json
      (Uri.with_path uri "/get/content")
      get_content_value
  in
  let v' = stored_value.data in
  Alcotest.(check store_content) "same stored content" value v'

let client () =
  let uri =
    try
      Uri.make ~scheme:"http" ~host:(Sys.getenv "SERVER_HOST") ~port:9090
        ~path:"graphql" ()
    with Not_found -> Uri.of_string "http://127.0.0.1:9090/graphql"
  in
  Alcotest.run ~and_exit:false "retirement-db-e2e"
    [
      ( "basic",
        [ Alcotest.test_case "get-and-set" `Quick (set_and_get_hash uri) ] );
    ]

let with_process (v, argv) f =
  let pid =
    Eio_unix.run_in_systhread (fun () ->
        Unix.create_process v argv Unix.stdin Unix.stdout Unix.stderr)
  in
  Fun.protect ~finally:(fun () -> Unix.kill pid Sys.sigkill) f

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
          "9090";
          "--verbosity";
          "info";
        |] )
      (fun () ->
        Eio_unix.sleep 2.;
        f ())
  in
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _token -> run client

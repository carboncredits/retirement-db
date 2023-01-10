open Eio
module Client = Cohttp_eio.Client
module Md = Multihash_digestif
module Store = Irmin_mem.Make (Retirement.Schema)

let headers = Http.Header.of_list [ ("Content-Type", "application/json") ]

let get_json ?headers ~net http (uri, resource) =
  let host, ip =
    match Uri.host uri with
    | None -> Fmt.failwith "Missing host in URL %a" Uri.pp uri
    | Some host -> (
        match (Unix.gethostbyname host).h_addr_list with
        | [||] -> Fmt.failwith "Unknown host %S in URL %a" host Uri.pp uri
        | arr -> (host, arr.(0))
        | exception Not_found ->
            Fmt.failwith "Unknown host %S in URL %a" host Uri.pp uri)
  in
  let port = Uri.port uri |> Option.value ~default:8080 in
  let addr = `Tcp (Eio_unix.Ipaddr.of_unix ip, port) in
  Switch.run @@ fun sw ->
  let conn = Net.connect ~sw net#net addr in
  let resp = http ?headers ~conn ("http://" ^ host) port resource in
  Client.read_fixed resp

let req_to_json ~net response ~host ~path body =
  let open Cohttp_eio in
  let http ?headers ~conn host _port path =
    Client.post ~conn ~body:(Body.Fixed body) ?headers net path ~host
  in
  let body = get_json ~headers ~net http (host, path) in
  response body

let store_content =
  Alcotest.of_pp (fun ppf v ->
      Fmt.pf ppf "%s" (Retirement_data.Json.string_of_t v))

let status =
  Alcotest.of_pp (fun ppf v ->
      Fmt.pf ppf "%s" (Retirement_data.Json.string_of_tx_status v))

(* let multihash = Alcotest.testable Md.pp Md.equal *)

module Rest = Retirement.Data.Rest

let set_and_get_hash ~clock ~net host () =
  let reason = "Test number 1" in
  let timestamp = Retirement.Data.current_ts clock in
  let value =
    Retirement.Data.v
      ~version:{ major = 0; minor = 1; patch = None }
      ~timestamp
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
  let content =
    req_to_json ~net Rest.Response.begin_tx_of_json ~host ~path:"/tx/begin"
      begin_value
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
  let stored_value =
    req_to_json ~net Rest.Response.get_content_of_json ~host
      ~path:"/get/content" get_content_value
  in
  let v' = stored_value.data in
  Alcotest.(check store_content) "same stored content" value v';
  let check_value =
    Retirement_data.Types.{ hash } |> Rest.Request.check_tx_status_to_json
  in
  let check =
    req_to_json ~net Rest.Response.check_tx_status_of_json ~host
      ~path:"/tx/status" check_value
  in
  Alcotest.(check status) "same tx status" `Pending check.data;
  let complete_value =
    Retirement_data.Types.{ hash; tx_id = "ABCDEFGH" }
    |> Rest.Request.complete_tx_to_json
  in
  let _check =
    req_to_json ~net Rest.Response.complete_tx_of_json ~host
      ~path:"/tx/complete" complete_value
  in
  let check_value =
    Retirement_data.Types.{ hash } |> Rest.Request.check_tx_status_to_json
  in
  let check =
    req_to_json ~net Rest.Response.check_tx_status_of_json ~host
      ~path:"/tx/status" check_value
  in
  Alcotest.(check bool)
    "same tx status" true
    (match check.data with `Complete _ -> true | _ -> false);
  (* This should fail because we've already added the value *)
  let begin_value =
    Retirement_data.Types.{ value } |> Rest.Request.begin_tx_to_json
  in
  let content =
    req_to_json ~net Rest.Response.begin_tx_of_json ~host ~path:"/tx/begin"
      begin_value
  in
  let errors = content.errors in
  Alcotest.(check int) "same errors" 1 (List.length errors)

let client clock net () =
  let uri =
    Uri.of_string
    @@ try Sys.getenv "SERVER_HOST" with Not_found -> "http://localhost:9090"
  in
  Alcotest.run ~and_exit:false "retirement-db-e2e"
    [
      ( "basic",
        [
          Alcotest.test_case "get-and-set" `Quick
            (set_and_get_hash ~clock ~net uri);
        ] );
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
          (* "--verbosity"; *)
          (* "debug"; *)
        |] )
      (fun () ->
        Eio_unix.sleep 2.;
        f ())
  in
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  run (client clock env)

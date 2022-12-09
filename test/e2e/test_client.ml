open Eio
module Client = Cohttp_eio.Client
module Md = Multihash_digestif
module Store = Irmin_mem.Make (Retirement.Schema)

let headers = Http.Header.of_list [ ("Content-Type", "application/json") ]
let ( / ) t s = Yojson.Safe.Util.member s t

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
  let conn = Net.connect ~sw net addr in
  let resp = http ?headers ~conn ("http://" ^ host) port resource in
  Client.read_fixed resp

let req_to_json ~net response ~host ~path body =
  let open Cohttp_eio in
  let http ?headers ~conn host port path =
    Client.post ~conn ~body:(Body.Fixed body) ?headers (host, Some port) path
  in
  let body = get_json ~headers ~net http (host, path) in
  response body

let store_content =
  Alcotest.of_pp (fun ppf v ->
      Fmt.pf ppf "%s" (Retirement_data.Json.string_of_t v))

(* let multihash = Alcotest.testable Md.pp Md.equal *)

module Rest = Retirement.Data.Rest

let set_and_get_hash ~net host () =
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
  let get_content_hash =
    Retirement_data.Types.{ value } |> Rest.Request.get_content_hash_to_json
  in
  let content_hash =
    req_to_json ~net Rest.Response.get_content_hash_of_json ~host
      ~path:"/get/content/hash" get_content_hash
  in
  let string_value =
    Retirement_data.Types.{ path = [ "hello" ]; value }
    |> Rest.Request.set_to_json
  in
  let content =
    req_to_json ~net Rest.Response.set_of_json ~host ~path:"/add" string_value
  in
  let commit = content.data in
  let get_hash_value =
    Retirement_data.Types.{ path = [ "hello" ]; commit }
    |> Rest.Request.get_hash_to_json
  in
  let store_value =
    req_to_json ~net Rest.Response.get_hash_of_json ~host ~path:"/get/hash"
      get_hash_value
  in
  let hash = store_value.data in
  let mh = Store.Contents.hash value in
  Alcotest.(check string)
    "same hash locally"
    (Irmin.Type.to_string Store.Hash.t mh)
    hash;
  Alcotest.(check string) "same hash remotely" content_hash.data hash;
  let get_content_value =
    Retirement_data.Types.{ hash } |> Rest.Request.get_content_to_json
  in
  let stored_value =
    req_to_json ~net Rest.Response.get_content_of_json ~host
      ~path:"/get/content" get_content_value
  in
  let v' = stored_value.data in
  Alcotest.(check store_content) "same stored content" value v'

let client net () =
  let uri =
    Uri.of_string
    @@ try Sys.getenv "SERVER_HOST" with Not_found -> "http://localhost:9090"
  in
  Alcotest.run ~and_exit:false "retirement-db-e2e"
    [
      ( "basic",
        [ Alcotest.test_case "get-and-set" `Quick (set_and_get_hash ~net uri) ]
      );
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
          "debug";
        |] )
      (fun () ->
        Eio_unix.sleep 2.;
        f ())
  in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  run (client net)

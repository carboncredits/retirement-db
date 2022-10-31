module Client = Cohttp_lwt_unix.Client
module Md = Multihash_digestif
module Store = Irmin_mem.Make (Retirement.Schema)

let await = Lwt_eio.Promise.await_lwt

(* Some pre-defined GraphQL queries *)
let get_by_hash_query hash =
  Fmt.str
    {|
  {
    contents(hash: "%s"){
      version {
        major
        minor
        patch
      }
      details {
        reasonText
      }
    }
  }
|}
    hash

let get_hash_from_commit_and_path ~commit ~path =
  Fmt.str
    {|
  {  
      commit(hash: "%s") {
          tree {
      get_contents(path: "%s") {
          hash
          value {
            version {
              major
              minor
              patch
            }
          }
      }}
      }
  }
|}
    commit path

let set_data =
  Fmt.str
    {|
  mutation {
    test_set_and_get(info: {parents: [], allow_empty: false, retries: 1, message: "Hello", author: "Me"}, set: $value, path: "hello/world", branch: "main") {
      hash
      info {
        message
      }
    }
  }
|}

let headers = Cohttp.Header.of_list [ ("Content-Type", "application/json") ]
let ( / ) t s = Yojson.Safe.Util.member s t

let graphql_req_to_json ?(variables = []) uri q =
  let body =
    `Assoc [ ("query", `String q); ("variables", `Assoc variables) ]
    |> Yojson.Safe.to_string
  in
  let _resp, body = await @@ Client.post ~body:(`String body) ~headers uri in
  let body = await @@ Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string body

let version =
  Alcotest.of_pp (fun ppf v ->
      Fmt.pf ppf "%s" (Retirement_data.Json.string_of_version v))

(* let multihash = Alcotest.testable Md.pp Md.equal *)

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
    Retirement.Data.to_json_string value |> Yojson.Safe.from_string
  in
  let content =
    graphql_req_to_json ~variables:[ ("value", string_value) ] uri set_data
  in
  let commit =
    content / "data" / "test_set_and_get" / "hash" |> Yojson.Safe.Util.to_string
  in
  let store_value =
    graphql_req_to_json uri
      (get_hash_from_commit_and_path ~commit ~path:"hello/world")
  in
  let hash =
    store_value / "data" / "commit" / "tree" / "get_contents" / "hash"
    |> function
    | `String s -> s
    | _ -> failwith "AHHHH"
  in
  let v =
    store_value / "data" / "commit" / "tree" / "get_contents" / "value"
    / "version"
    |> Yojson.Safe.to_string
  in
  let v = Retirement_data.Json.version_of_string v in
  let mh = Store.Contents.hash value in
  Alcotest.(check string)
    "same hash"
    (Irmin.Type.to_string Store.Hash.t mh)
    hash;
  let value' = graphql_req_to_json uri (get_by_hash_query hash) in
  let reason' =
    value' / "data" / "contents" / "details" / "reasonText"
    |> Yojson.Safe.Util.to_string
  in
  Alcotest.(check string) "same reason text" reason reason';
  Alcotest.(check version)
    "same version number" Retirement_data.latest_version v

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

module Client = Cohttp_lwt_unix.Client

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

let set_data ~reason =
  Fmt.str
    {|
  mutation {
    test_set_and_get(info: {parents: [], allow_empty: false, retries: 1, message: "Hello", author: "Me"}, set: {version: { major: 0, minor: 1 }, 
      details: {
          flightDetails: [],
          trainDetails: [],
          taxiDetails: [],
          additionalDetails: [],
          primaryReason: "Conference",
          reasonText: "%s"
      }
  }, path: "hello/world", branch: "main") {
      hash
      info {
        message
      }
    }
  }
|}
    reason

let headers = Cohttp.Header.of_list [ ("Content-Type", "application/json") ]
let ( / ) t s = Yojson.Safe.Util.member s t

let graphql_req_to_json uri q =
  let body = `Assoc [ ("query", `String q) ] |> Yojson.Safe.to_string in
  let _resp, body = await @@ Client.post ~body:(`String body) ~headers uri in
  let body = await @@ Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string body

let version =
  Alcotest.of_pp (fun ppf v ->
      Fmt.pf ppf "%s" (Retirement_data.Json.string_of_version v))

let set_and_get_hash uri () =
  let reason = "Test number 1" in
  let content = graphql_req_to_json uri (set_data ~reason) in
  let commit =
    content / "data" / "test_set_and_get" / "hash" |> Yojson.Safe.Util.to_string
  in
  let value =
    graphql_req_to_json uri
      (get_hash_from_commit_and_path ~commit ~path:"hello/world")
  in
  let hash =
    value / "data" / "commit" / "tree" / "get_contents" / "hash"
    |> Yojson.Safe.Util.to_string
  in
  let v =
    value / "data" / "commit" / "tree" / "get_contents" / "value" / "version"
    |> Yojson.Safe.to_string
  in
  print_endline v;
  let v = Retirement_data.Json.version_of_string v in
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

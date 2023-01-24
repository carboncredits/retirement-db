open Retirement

module Irmin_store = struct
  include Irmin_mem.Make (Retirement.Schema)
end

module Store = Store.Make (Irmin_store)

let mock_clock = Eio_mock.Clock.make ()

let projects =
  Eio_mock.Clock.set_time mock_clock 1.;
  let v1 = Data.dummy_details (mock_clock :> Eio.Time.clock) in
  Eio_mock.Clock.set_time mock_clock 2.;
  let v2 = Data.dummy_details (mock_clock :> Eio.Time.clock) in
  [ v1; v2 ]

let lookup_projects =
  Eio_mock.Clock.set_time mock_clock 1.;
  let v1 =
    {
      (Data.dummy_details (mock_clock :> Eio.Time.clock)) with
      booker_crsid = "abc";
    }
  in
  Eio_mock.Clock.set_time mock_clock 2.;
  let v2 =
    {
      (Data.dummy_details (mock_clock :> Eio.Time.clock)) with
      booker_crsid = "abc";
    }
  in
  Eio_mock.Clock.set_time mock_clock 3.;
  let v3 =
    {
      (Data.dummy_details (mock_clock :> Eio.Time.clock)) with
      booker_crsid = "abc";
    }
  in
  Eio_mock.Clock.set_time mock_clock 4.;
  let v4 =
    {
      (Data.dummy_details (mock_clock :> Eio.Time.clock)) with
      booker_crsid = "def";
    }
  in
  [ v1; v2; v3; v4 ]

let tx =
  let txid = ref 0 in
  fun () ->
    incr txid;
    string_of_int !txid

let add ~clock t v =
  match Store.begin_transaction t ~clock v with
  | Ok hash -> (
      let tx = tx () in
      match Store.complete_transaction t ~clock ~hash ~tx with
      | Ok _c -> ()
      | Error (`Msg e) -> failwith e
      | Error `Not_pending -> failwith "Not pending")
  | Error tx -> Store.tx_error_to_string tx |> failwith

let add_items ~clock store items = List.iter (add ~clock store) items

let with_store ?(empty = true) ~name ~clock fn =
  (* Ensure config uniqueness! *)
  let root = Irmin.Backend.Conf.root Irmin_mem.Conf.spec in
  let config =
    Irmin_mem.config () |> fun c -> Irmin.Backend.Conf.add c root name
  in
  Option.iter
    (fun v -> Printf.printf "ROOT: %s\n%!" v)
    (Irmin.Backend.Conf.find_root config);
  let store = Store.v config in
  if not empty then add_items ~clock store projects;
  fn store

let project = Alcotest.of_pp Data.pp

(* The serialisation functions in the `project.ml` file should be able
   to read the various versions of the project metadata format. Any time
   we bump a version we should add it to test/versions and ensure it can
   be read here. *)
let test_backwards_compat dir =
  let files = Eio.Path.read_dir dir in
  List.map
    (fun file ->
      Alcotest.test_case file `Quick @@ fun () ->
      let s = Eio.Path.(load (dir / file)) in
      let project = Data.of_string s in
      match project with
      | Ok _ -> ()
      | Error (`Msg s) -> Alcotest.fail ("Parsing failed with: " ^ s))
    files

let test_read ~clock () =
  with_store ~clock ~empty:false ~name:"test-read" @@ fun main ->
  let proj_1 = List.hd projects in
  let path = Data.get_path ~digest:Store.hash_content proj_1 in
  let updated = { proj_1 with tx_id = Some "1" } in
  let proj = Option.get @@ Store.lookup_transacted main path in
  Alcotest.check project "same project" updated proj

let test_get_all ~clock () =
  with_store ~clock ~empty:true ~name:"test-get-all2" @@ fun main ->
  add_items ~clock main projects;
  let with_txid =
    List.mapi
      (fun i (v : Data.t) -> { v with tx_id = Some (string_of_int (i + 3)) })
      projects
  in
  let projs = Store.lookup_all_transacted main [ "1970"; "1" ] in
  Alcotest.(check (list project)) "same projects" with_txid projs

let test_lookup ~clock () =
  with_store ~clock ~empty:true ~name:"test-lookup" @@ fun main ->
  add_items ~clock main lookup_projects;
  let current_year, current_month, _ =
    Ptime.of_float_s (Unix.gettimeofday ()) |> Option.get |> Ptime.to_date
  in
  let projects =
    List.filter
      (fun v -> v.Retirement_data.Types.booker_crsid = "abc")
      lookup_projects
  in
  let with_txid =
    List.mapi
      (fun i (v : Data.t) -> { v with tx_id = Some (string_of_int (i + 5)) })
      projects
  in
  let none =
    Store.lookup_bookers_transacted ~booker:"abc" ~current_year ~current_month
      ~months:1 main
  in
  Alcotest.(check (list project)) "no projects" [] none;
  let projs =
    Store.lookup_bookers_transacted ~booker:"abc" ~current_year:1970
      ~current_month:1 ~months:1 main
  in
  Alcotest.(check (list project)) "same projects" with_txid projs

let () =
  let test_case s fn = Alcotest.test_case s `Quick fn in
  Eio_main.run @@ fun env ->
  let dir =
    let cwd = Eio.Stdenv.cwd env in
    Eio.Path.(cwd / "versions")
  in
  let clock = Eio.Stdenv.clock env in
  Alcotest.run "database"
    [
      ( "basics",
        [
          test_case "read" (test_read ~clock);
          test_case "get-all" (test_get_all ~clock);
          test_case "lookup" (test_lookup ~clock);
        ] );
      ("low-level", test_backwards_compat dir);
    ]

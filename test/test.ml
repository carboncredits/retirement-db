open Lwt.Syntax
open Retirement

module Irmin_store = struct
  include Irmin_mem.Make (Retirement.Schema)
end

module Store = Store.Make (Irmin_store)

type clock = < float Data.Time.clock_base ; set_time : float -> unit >

let mock_clock : clock =
  object
    val mutable clock = 0.
    method now = clock
    method set_time f = clock <- f
  end

let set_time (c : clock) = c#set_time

let projects =
  set_time mock_clock 1.;
  let v1 = Data.dummy_details (mock_clock :> Data.Time.clock) in
  set_time mock_clock 2.;
  let v2 = Data.dummy_details (mock_clock :> Data.Time.clock) in
  [ v1; v2 ]

let lookup_projects =
  set_time mock_clock 1.;
  let v1 = { (Data.dummy_details mock_clock) with booker_crsid = "abc" } in
  set_time mock_clock 2.;
  let v2 = { (Data.dummy_details mock_clock) with booker_crsid = "abc" } in
  set_time mock_clock 3.;
  let v3 = { (Data.dummy_details mock_clock) with booker_crsid = "abc" } in
  set_time mock_clock 4.;
  let v4 = { (Data.dummy_details mock_clock) with booker_crsid = "def" } in
  [ v1; v2; v3; v4 ]

let tx =
  let txid = ref 0 in
  fun () ->
    incr txid;
    string_of_int !txid

let add ~clock t v =
  let* btx = Store.begin_transaction t ~clock v in
  match btx with
  | Ok hash -> (
      let tx = tx () in
      let+ comp = Store.complete_transaction t ~clock ~hash ~tx in
      match comp with
      | Ok _c -> ()
      | Error (`Msg e) -> failwith e
      | Error `Not_pending -> failwith "Not pending")
  | Error tx -> Store.tx_error_to_string tx |> failwith

let add_items ~clock store items = Lwt_list.iter_s (add ~clock store) items

let with_store ?(empty = true) ~name ~clock fn =
  (* Ensure config uniqueness! *)
  let root = Irmin.Backend.Conf.root Irmin_mem.Conf.spec in
  let config =
    Irmin_mem.config () |> fun c -> Irmin.Backend.Conf.add c root name
  in
  Option.iter
    (fun v -> Printf.printf "ROOT: %s\n%!" v)
    (Irmin.Backend.Conf.find_root config);
  let* store = Store.v config in
  let* () =
    if not empty then add_items ~clock store projects else Lwt.return_unit
  in
  fn store

let project = Alcotest.of_pp Data.pp

(* The serialisation functions in the `project.ml` file should be able
   to read the various versions of the project metadata format. Any time
   we bump a version we should add it to test/versions and ensure it can
   be read here. *)
let test_backwards_compat dir =
  let files = Sys.readdir dir |> Array.to_list in
  List.map
    (fun file ->
      Alcotest.test_case file `Quick @@ fun () ->
      let s =
        In_channel.with_open_bin (Filename.concat dir file) In_channel.input_all
      in
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
  let+ lookup = Store.lookup_transacted main path in
  let proj = Option.get lookup in
  Alcotest.check project "same project" updated proj

let test_get_all ~clock () =
  with_store ~clock ~empty:true ~name:"test-get-all2" @@ fun main ->
  let* () = add_items ~clock main projects in
  let with_txid =
    List.mapi
      (fun i (v : Data.t) -> { v with tx_id = Some (string_of_int (i + 3)) })
      projects
  in
  let+ projs = Store.lookup_all_transacted main [ "1970"; "1" ] in
  Alcotest.(check (list project)) "same projects" with_txid projs

let test_lookup ~clock () =
  with_store ~clock ~empty:true ~name:"test-lookup" @@ fun main ->
  let* () = add_items ~clock main lookup_projects in
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
  let* none =
    Store.lookup_bookers_transacted ~booker:"abc" ~current_year ~current_month
      ~months:1 main
  in
  Alcotest.(check (list project)) "no projects" [] none;
  let+ projs =
    Store.lookup_bookers_transacted ~booker:"abc" ~current_year:1970
      ~current_month:1 ~months:1 main
  in
  Alcotest.(check (list project)) "same projects" with_txid projs

let () =
  let test_case s fn = Alcotest.test_case s `Quick fn in
  let dir =
    let cwd = Sys.getcwd () in
    Filename.concat cwd "versions"
  in
  let clock =
    object
      method now = Unix.gettimeofday ()
    end
  in
  let run_lwt fn () = Lwt_main.run (fn ()) in
  Alcotest.run "database"
    [
      ( "basics",
        [
          test_case "read" (run_lwt @@ test_read ~clock);
          test_case "get-all" (run_lwt @@ test_get_all ~clock);
          test_case "lookup" (run_lwt @@ test_lookup ~clock);
        ] );
      ("low-level", test_backwards_compat dir);
    ]

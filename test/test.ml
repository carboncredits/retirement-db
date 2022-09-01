open Retirement
module Git_store = Git.Mem.Store

module Schema =
  Irmin_git.Schema.Make (Git_store) (Data)
    (Irmin_git.Branch.Make (Irmin.Branch.String))

module Git_mem = Irmin_git.Maker (Irmin_git.Mem) (Git.Mem.Sync (Git_store))

module Irmin_store = struct
  module Schema = Schema
  include Git_mem.Make (Schema)
end

module Store = Store.Make (Irmin_store)

let dummy_details =
  Retirement_data.Types.
    {
      flight_trip_type = `None;
      outbound_details = None;
      inbound_details = None;
      train_details = [];
      taxi_details = [];
      additional_details = [];
      primary_reason = `Conference;
      secondary_reason = None;
      reason_text = "Some reason for travelling!";
    }

let projects =
  [ ([ "a" ], Data.v dummy_details); ([ "b" ], Data.v dummy_details) ]

let add_projects store =
  List.iter
    (fun (path, proj) -> Store.add_project store path proj |> Result.get_ok)
    projects

let with_store ?(empty = true) fn =
  let config = Irmin_mem.config () in
  let repo = Store.repository config in
  let main = Store.of_branch repo in
  if empty then fn main else add_projects main;
  fn main

let project = Alcotest.of_pp Data.pp

(* The serialisation functions in the `project.ml` file should be able
   to read the various versions of the project metadata format. Any time
   we bump a version we should add it to test/versions and ensure it can
   be read here. *)
let test_backwards_compat dir () =
  let files = Eio.Path.read_dir dir in
  List.iter
    (fun file ->
      let s = Eio.Path.(load (dir / file)) in
      let project = Data.of_string s in
      match project with
      | Ok _ -> ()
      | Error (`Msg s) -> Alcotest.fail ("Parsing failed with: " ^ s))
    files

let test_read () =
  with_store ~empty:false @@ fun main ->
  let path_1, proj_1 = List.hd projects in
  let proj = Store.get_project main path_1 in
  Alcotest.check project "same project" proj_1 proj

let () =
  let test_case s fn = Alcotest.test_case s `Quick fn in
  Eio_main.run @@ fun env ->
  let dir =
    let cwd = Eio.Stdenv.cwd env in
    Eio.Path.(cwd / "versions")
  in
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _id ->
  Alcotest.run "project database"
    [
      ("basics", [ test_case "read" test_read ]);
      ("low-level", [ test_case "backwards-compat" (test_backwards_compat dir) ]);
    ]

open Retirement

module Irmin_store = struct
  include Irmin_mem.Make (Retirement.Schema)
end

module Store = Store.Make (Irmin_store)

let projects = [ ([ "a" ], Data.dummy_details); ([ "b" ], Data.dummy_details) ]

let same_path_projects =
  [
    ([ "2022"; "1"; "a" ], Data.dummy_details);
    ([ "2022"; "1"; "b" ], Data.dummy_details);
  ]

let add_projects ~clock store projects =
  List.map
    (fun (path, proj) ->
      Store.add_project ~clock store path proj |> Result.get_ok)
    projects
  |> ignore

let with_store ?(empty = true) ~clock fn =
  let config = Irmin_mem.config () in
  let repo = Store.repository config in
  let main = Store.of_branch repo in
  if empty then fn main else add_projects ~clock main projects;
  fn main

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
  with_store ~clock ~empty:false @@ fun main ->
  let path_1, proj_1 = List.hd projects in
  let proj = Store.get_project main path_1 in
  Alcotest.check project "same project" proj_1 proj

let test_get_all ~clock () =
  with_store ~clock ~empty:true @@ fun main ->
  add_projects ~clock main same_path_projects;
  let real = List.map snd same_path_projects in
  let projs = Store.get_all main [ "2022"; "1" ] in
  Alcotest.(check (list project)) "same projects" real projs

let () =
  let test_case s fn = Alcotest.test_case s `Quick fn in
  Eio_main.run @@ fun env ->
  let dir =
    let cwd = Eio.Stdenv.cwd env in
    Eio.Path.(cwd / "versions")
  in
  let clock = Eio.Stdenv.clock env in
  Alcotest.run "project database"
    [
      ( "basics",
        [
          test_case "read" (test_read ~clock);
          test_case "get-all" (test_get_all ~clock);
        ] );
      ("low-level", test_backwards_compat dir);
    ]

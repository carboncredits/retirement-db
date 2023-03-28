open Lwt.Syntax
open Retirement

module I = struct
  include Irmin_fs_unix.Make (Retirement.Schema)
end

module Store = Store.Make (I)

let with_store fn =
  let config = Irmin_fs.config "./tmp" in
  let s = Store.v config in
  Fun.protect
    (fun () ->
      let* s = s in
      fn s)
    ~finally:(fun () -> Sys.command "rm -rf ./tmp" |> ignore)

let check_content ~hash store =
  let+ find = Store.find store ~hash in
  assert (Option.is_some find)

let concurrent_completions ~clock =
  with_store @@ fun store ->
  let details = Retirement.Data.dummy_details clock in
  let* id = Store.begin_transaction ~clock store details in
  match id with
  | Error _ -> failwith "Begin tx failed!"
  | Ok s ->
      let* () = check_content ~hash:s store in
      Fmt.pr "Successfully started tx: %s" s;
      let complete () =
        Store.complete_transaction ~clock store ~hash:s ~tx:"WOOOHOOOO"
      in
      let res = Lwt_list.map_p complete [ (); () ] in
      res

let mock_clock =
  object
    method now = 0.
  end

let () =
  let main =
    Fmt.pr "<><><><><><><><> Concurrency Test <><><><><><><><>\n";
    let+ res = concurrent_completions ~clock:mock_clock in
    match res with
    | [ Ok _; Error _ ] -> Fmt.pr "Concurrent Test Passed\n"
    | [ Error _; Ok _ ] -> Fmt.pr "Concurrent Test Passed\n"
    | _ -> failwith "Store concurrency is broken!"
  in
  Lwt_main.run main

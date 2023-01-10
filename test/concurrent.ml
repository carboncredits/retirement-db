open Retirement

module I = struct
  include Irmin_fs_unix.Make (Retirement.Schema)
end

module Store = Store.Make (I)

let with_store ~fs:_ fn =
  let config = Irmin_fs.config "./tmp" in
  let s = Store.v config in
  Fun.protect
    (fun () -> fn s)
    ~finally:(fun () -> Sys.command "rm -rf ./tmp" |> ignore)

let check_content ~hash store =
  assert (Option.is_some @@ Store.find store ~hash)

let concurrent_completions ~clock fs =
  with_store ~fs @@ fun store ->
  let details = Retirement.Data.dummy_details clock in
  match Store.begin_transaction ~clock store details with
  | Error _ -> failwith "Begin tx failed!"
  | Ok s ->
      check_content ~hash:s store;
      Eio.traceln "Successfully started tx: %s" s;
      let complete () =
        Store.complete_transaction ~clock store ~hash:s ~tx:"WOOOHOOOO"
      in
      let res = Eio.Fiber.List.map ~max_fibers:10 complete [ (); () ] in
      res

let () =
  Eio_main.run @@ fun env ->
  Eio.traceln "<><><><><><><><> Concurrency Test <><><><><><><><>\n";
  let clock = Eio_mock.Clock.make () in
  Irmin_fs.run env#fs @@ fun () ->
  let res = concurrent_completions ~clock:(clock :> Eio.Time.clock) env#fs in
  match res with
  | [ Ok _; Error _ ] -> Eio.traceln "Concurrent Test Passed\n"
  | [ Error _; Ok _ ] -> Eio.traceln "Concurrent Test Passed\n"
  | _ -> failwith "Store concurrency is broken!"

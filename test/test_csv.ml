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

let flight : Retirement_data.Types.flight_details =
  {
    date = "2023-02-09T15:48:10.801Z";
    arrival = { iata_code = "BFS"; name = "Belfast Intl."; id = "BFS" };
    departure = { iata_code = "LHR"; name = "London Heathrow"; id = "LHR" };
    aircraft_type = Some "A320";
    flight_count = 2;
    passenger_count = 2;
    travel_class = Some "blah";
    charter = None;
  }

let projects =
  set_time mock_clock 1.;
  let v1 = Data.dummy_details mock_clock in
  let v1 =
    {
      v1 with
      details = { v1.details with flight_details = [ flight; flight; flight ] };
    }
  in
  set_time mock_clock 2.;
  let v2 = Data.dummy_details mock_clock in
  let v2 =
    {
      v2 with
      details = { v2.details with flight_details = [ flight ] };
      booker_crsid = "pf341";
    }
  in
  [ v1; v2 ]

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
  (* Option.iter
     (fun v -> Printf.printf "ROOT: %s\n%!" v)
     (Irmin.Backend.Conf.find_root config); *)
  let* store = Store.v config in
  let* () =
    if not empty then add_items ~clock store projects else Lwt.return_unit
  in
  fn store

let () =
  let clock =
    object
      method now = Unix.gettimeofday ()
    end
  in
  Lwt_main.run
  @@ with_store ~name:"csv-store" ~clock
  @@ fun s ->
  let* () = add_items ~clock s projects in
  let* csv_flights = Store.csv_by_flight s ~year:1970 ~month:1 in
  let+ csv_finance = Store.csv_by_finance s ~year:1970 ~month:1 in
  Printf.printf "%s\n\n%!" csv_flights;
  Printf.printf "%s%!" csv_finance

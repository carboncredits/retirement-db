open Retirement

module Irmin_store = struct
  include Irmin_mem.Make (Retirement.Schema)
end

module Store = Store.Make (Irmin_store)

let mock_clock = Eio_mock.Clock.make ()

let flight : Retirement_data.Types.flight_details =
  {
    arrival = { iata_code = "BFS"; name = "Belfast Intl."; id = "BFS" };
    departure = { iata_code = "LHR"; name = "London Heathrow"; id = "LHR" };
    aircraft_type = Some "A320";
    flight_count = 2;
    passenger_count = 2;
    travel_class = Some "blah";
    charter = None;
  }

let projects =
  Eio_mock.Clock.set_time mock_clock 1.;
  let v1 = Data.dummy_details (mock_clock :> Eio.Time.clock) in
  let v1 =
    {
      v1 with
      details = { v1.details with flight_details = [ flight; flight; flight ] };
    }
  in
  Eio_mock.Clock.set_time mock_clock 2.;
  let v2 = Data.dummy_details (mock_clock :> Eio.Time.clock) in
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
  (* Option.iter
     (fun v -> Printf.printf "ROOT: %s\n%!" v)
     (Irmin.Backend.Conf.find_root config); *)
  let store = Store.v config in
  if not empty then add_items ~clock store projects;
  fn store

let () =
  Eio_main.run @@ fun env ->
  with_store ~name:"csv-store" ~clock:env#clock @@ fun s ->
  add_items ~clock:env#clock s projects;
  Printf.printf "%s\n\n%!" (Store.csv_by_flight s ~year:1970 ~month:1);
  Printf.printf "%s%!" (Store.csv_by_finance s ~year:1970 ~month:1)

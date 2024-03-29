module T = Retirement_data.Json
module J = Retirement_data.Json

type t = T.t

let wrap_exn f v =
  try Ok (f v) with exn -> Error (`Msg (Printexc.to_string exn))

let t =
  let of_string s = wrap_exn J.t_of_string s in
  let pp ppf (v : t) =
    let buffer = Buffer.create 128 in
    J.write_t buffer v;
    Buffer.contents buffer |> Fmt.string ppf
  in
  let json =
    let enc e v =
      let dst = Jsonm.encoder_dst e in
      match dst with
      | `Buffer buffer -> J.write_t buffer v
      | `Channel oc ->
          let buff = Buffer.create 128 in
          J.write_t buff v;
          Buffer.output_buffer oc buff
      | _ -> failwith "Manual dst unsupported"
    in
    let dec d =
      let src =
        Irmin.Type.Json.decoder_and_lexemes d |> fst |> Jsonm.decoder_src
      in
      match src with
      | `String s -> wrap_exn J.t_of_string s
      | `Channel ic ->
          let lexbuf = Lexing.from_channel ic in
          let state = Yojson.init_lexer () in
          wrap_exn (J.read_t state) lexbuf
      | `Manual -> Error (`Msg "Manual src not supported")
    in
    (enc, dec)
  in
  let bin : t Repr.encode_bin * t Repr.decode_bin * t Repr.size_of =
    let encode_bin (v : t) (f : string -> unit) : unit = f (J.string_of_t v) in
    let decode_bin (s : string) (i : int ref) : t =
      let v = J.t_of_string s in
      i := !i + String.length s;
      v
    in
    let size = Irmin.Type.Size.t T.t in
    (encode_bin, decode_bin, size)
  in
  let unboxed_bin : t Repr.encode_bin * t Repr.decode_bin * t Repr.size_of =
    let encode_bin (v : t) (f : string -> unit) : unit = f (J.string_of_t v) in
    let decode_bin (s : string) (i : int ref) : t =
      let v = J.t_of_string s in
      i := !i + String.length s;
      v
    in
    let size = Irmin.Type.Size.t T.t in
    (encode_bin, decode_bin, size)
  in
  Irmin.Type.like ~unboxed_bin ~bin ~compare ~json ~pp ~of_string T.t

let version (t : T.t) = t.version

let raw_version s =
  match Yojson.Safe.from_string s with
  | `Assoc assoc ->
      let version = List.assoc_opt "version" assoc in
      Option.map
        (fun json -> J.version_of_string (Yojson.Safe.to_string json))
        version
  | _ -> None

let pp ppf t = Fmt.pf ppf "%s" (J.string_of_t t)
let of_string v = try Ok (J.t_of_string v) with Failure s -> Error (`Msg s)
let to_json_string d = J.string_of_t d
let list_to_json_string ds = J.string_of_t_list ds
let to_pretty_string d = to_json_string d |> Yojson.Safe.prettify

type finance_details =
  [ `Grant of Retirement_data.Types.grant_details
  | `CostCentre of Retirement_data.Types.cost_centre_details ]

module Time = struct
  class virtual ['a] clock_base =
    object
      method virtual now : 'a
    end

  class virtual clock =
    object
      inherit [float] clock_base
    end

  let now (c : #clock) = c#now
end

let current_ts clock =
  Time.now clock |> Ptime.of_float_s |> Option.get |> Ptime.to_rfc3339

let ts_to_date ts =
  let t, _, _ = Ptime.of_rfc3339 ts |> Result.get_ok in
  Ptime.to_date t

let timestamp t =
  let t, _, _ = Ptime.of_rfc3339 t.Retirement_data.Types.ts |> Result.get_ok in
  t

let get_path ~digest (t : t) =
  let year, month, _ = ts_to_date t.ts in
  [ string_of_int year; string_of_int month; digest t ]

module Flight_csv = struct
  (* A subset of the data that is CSV-able *)
  module Line = struct
    type t = {
      booker_crsid : string;
      business_traveller_crsid : string;
      departure : string;
      arrival : string;
      passenger_count : int;
      flight_count : int;
      ts : string;
      tx_id : string;
    }
    [@@deriving fields, csv]
  end

  let to_csv (v : Retirement_data.Types.t) =
    let flights = v.details.flight_details in
    let line (t : J.t) (v : J.flight_details) =
      match t.tx_id with
      | Some tx_id ->
          Some
            {
              Line.booker_crsid = t.booker_crsid;
              business_traveller_crsid = t.business_traveller.crsid;
              departure = v.departure.iata_code;
              arrival = v.arrival.iata_code;
              passenger_count = v.passenger_count;
              flight_count = v.flight_count;
              ts = t.ts;
              tx_id;
            }
      | None -> None
    in
    List.filter_map (line v) flights
end

module Finance_csv = struct
  module Line = struct
    type t = {
      booker_crsid : string;
      business_traveller_crsid : string;
      total_flights : int;
      co2e_amount : int;
      ts : string;
      tx_id : string;
    }
    [@@deriving fields, csv]
  end

  let to_csv (v : Retirement_data.Types.t) =
    let total_flights =
      List.fold_left
        (fun acc (v : J.flight_details) -> acc + v.flight_count)
        0 v.details.flight_details
    in
    match v.tx_id with
    | Some tx_id ->
        Some
          {
            Line.booker_crsid = v.booker_crsid;
            business_traveller_crsid = v.business_traveller.crsid;
            total_flights;
            co2e_amount = v.offset.amount;
            ts = v.ts;
            tx_id;
          }
    | None -> None
end

let v ?(version = Retirement_data.latest_version) ?tx_id ~timestamp booker_crsid
    business_traveller finance details offset =
  match finance with
  | `Grant d ->
      {
        T.version;
        ts = timestamp;
        details;
        booker_crsid;
        business_traveller;
        offset;
        finance_kind = `Grant;
        tx_id;
        grant_details = Some d;
        cost_centre_details = None;
      }
  | `CostCentre d ->
      {
        T.version;
        ts = timestamp;
        details;
        booker_crsid;
        business_traveller;
        offset;
        tx_id;
        finance_kind = `CostCentre;
        grant_details = None;
        cost_centre_details = Some d;
      }

let dummy_travel_details =
  Retirement_data.Types.
    {
      flight_details =
        [
          {
            date = "2023-02-09T15:48:10.801Z";
            departure = { iata_code = "BFS"; id = "BFS"; name = "Belfast" };
            arrival = { iata_code = "LHR"; id = "LHR"; name = "London" };
            aircraft_type = None;
            passenger_count = 1;
            travel_class = None;
            flight_count = 1;
            charter = None;
          };
        ];
      train_details = [];
      taxi_details = [];
      additional_details = [];
      primary_reason = `Conference;
      secondary_reason = None;
      reason_text = "Some reason for travelling!";
    }

let dummy_grant_details =
  Retirement_data.Types.
    {
      sponsor_and_pi_confirmation = true;
      award = "award";
      project = "project";
      task = "task";
    }

let dummy_offset =
  Retirement_data.Types.
    {
      token_id = 1234;
      project_name = "Gola";
      minter = "abcd1234wxyz5678";
      kyc = "1234abcd5678wxyz";
      amount = 556789;
    }

let dummy_details ?tx_id ?timestamp clock =
  let timestamp =
    match timestamp with Some t -> t | None -> current_ts clock
  in
  v ~timestamp ?tx_id "xyz123"
    { crsid = "abc123"; department = "CST"; name = "Alice" }
    (`Grant dummy_grant_details) dummy_travel_details dummy_offset

let details t = t.T.details
let merge' ~old:_ t _t2 = Lwt.return @@ Ok t
let merge = Irmin.Merge.(option (v t merge'))

module Rest = struct
  module Request = struct
    type begin_tx = Retirement_data.Types.begin_tx_request

    let begin_tx_to_json = Retirement_data.Json.string_of_begin_tx_request

    type complete_tx = Retirement_data.Types.complete_tx_request

    let complete_tx_to_json = Retirement_data.Json.string_of_complete_tx_request

    type check_tx_status = Retirement_data.Types.check_tx_status_request

    let check_tx_status_to_json =
      Retirement_data.Json.string_of_check_tx_status_request

    type get_hash = Retirement_data.Types.get_hash_request

    let get_hash_to_json = Retirement_data.Json.string_of_get_hash_request

    type get_content = Retirement_data.Types.get_content_request

    let get_content_to_json = Retirement_data.Json.string_of_get_content_request

    type get_bookers = Retirement_data.Types.get_bookers_request

    let get_bookers_to_json = Retirement_data.Json.string_of_get_bookers_request
  end

  module Response = struct
    type begin_tx = Retirement_data.Types.string_response
    (** Returns the hash of the value. *)

    let begin_tx_to_json = Retirement_data.Json.string_of_string_response
    let begin_tx_of_json = Retirement_data.Json.string_response_of_string

    type complete_tx = Retirement_data.Types.string_response
    (** Returns the hash of the latest merged commit in the transaction store. *)

    let complete_tx_to_json = Retirement_data.Json.string_of_string_response
    let complete_tx_of_json = Retirement_data.Json.string_response_of_string

    type check_tx_status = Retirement_data.Types.tx_status_response

    let check_tx_status_to_json =
      Retirement_data.Json.string_of_tx_status_response

    let check_tx_status_of_json =
      Retirement_data.Json.tx_status_response_of_string

    type get_hash = Retirement_data.Types.string_response

    let get_hash_to_json = Retirement_data.Json.string_of_string_response
    let get_hash_of_json = Retirement_data.Json.string_response_of_string

    type get_content = Retirement_data.Types.t_response
    (** The result of setting a new value in the store, returns the hash of the value. *)

    let get_content_to_json = Retirement_data.Json.string_of_t_response
    let get_content_of_json = Retirement_data.Json.t_response_of_string

    type get_bookers = Retirement_data.Types.t_list_response

    let get_bookers_to_json = Retirement_data.Json.string_of_t_list_response
    let get_bookers_of_json = Retirement_data.Json.t_list_response_of_string
  end
end

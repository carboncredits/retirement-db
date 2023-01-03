module T = Retirement_data.Json
module J = Retirement_data.Json

type t = T.t

let t = T.t
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

let current_ts clock =
  Eio.Time.now clock |> Ptime.of_float_s |> Option.get |> Ptime.to_rfc3339

let ts_to_date ts =
  let t, _, _ = Ptime.of_rfc3339 ts |> Result.get_ok in
  Ptime.to_date t

let get_path ~digest (t : t) =
  let year, month, _ = ts_to_date t.ts in
  [ string_of_int year; string_of_int month; digest t ]

let v ?(version = Retirement_data.latest_version) ?tx_id ~timestamp id finance
    details offset =
  match finance with
  | `Grant d ->
      {
        T.version;
        ts = timestamp;
        details;
        id;
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
        id;
        offset;
        tx_id;
        finance_kind = `CostCentre;
        grant_details = None;
        cost_centre_details = Some d;
      }

let dummy_travel_details =
  Retirement_data.Types.
    {
      flight_details = [];
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
  v ~timestamp ?tx_id
    { crsid = "abc123"; department = "CST"; name = "Alice" }
    (`Grant dummy_grant_details) dummy_travel_details dummy_offset

let details t = t.T.details
let merge' ~old:_ t1 _t2 = Ok t1
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
  end
end

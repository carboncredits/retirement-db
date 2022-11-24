module T = Retirement_data.Json
module J = Retirement_data.Json

(* let wrap f v = try Ok (f v) with _ -> Error (`Msg "Serialisation failed") *)

(* let json_serial ~of_string ~pp t =
   let pp ppf v =
     let buff = Buffer.create 1028 in
     pp buff v;
     Fmt.string ppf (Buffer.contents buff)
   in
   Irmin.Type.like ~of_string:(wrap of_string) ~pp t *)

type t = T.t

let t = T.t
let schema_typ = T.schema_typ
let arg_typ = T.arg_typ
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

let v ?(version = Retirement_data.latest_version) ~ts id finance details offset
    =
  match finance with
  | `Grant d ->
      {
        T.version;
        ts;
        details;
        id;
        offset;
        finance_kind = `Grant;
        grant_details = Some d;
        cost_centre_details = None;
      }
  | `CostCentre d ->
      {
        T.version;
        ts;
        details;
        id;
        offset;
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

let dummy_details =
  v ~ts:"2022-11-24T10:53:06-00:00"
    { crsid = "abc123"; department = "CST"; name = "Alice" }
    (`Grant dummy_grant_details) dummy_travel_details dummy_offset

let details t = t.T.details
let merge' ~old:_ t1 _t2 = Lwt.return (Ok t1)
let merge = Irmin.Merge.(option (v t merge'))

module T = Retirement_data.Json
module J = Retirement_data.Json

let wrap f v = try Ok (f v) with _ -> Error (`Msg "Serialisation failed")

let json_serial ~of_string ~pp t =
  let pp ppf v =
    let buff = Buffer.create 1028 in
    pp buff v;
    Fmt.string ppf (Buffer.contents buff)
  in
  Irmin.Type.like ~of_string:(wrap of_string) ~pp t

type t = T.t

let t = json_serial ~of_string:J.t_of_string ~pp:J.write_t T.t
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
let to_pretty_string d = to_json_string d |> Yojson.Safe.prettify

let v ?(version = Retirement_data.latest_version) details =
  { T.version; details }

let details t = t.T.details
let merge' ~old:_ t1 _t2 = Lwt.return (Ok t1)
let merge = Irmin.Merge.(option (v t merge'))

include Retirement_data.Json

let version (t : t) = t.version

let raw_version s =
  match Yojson.Safe.from_string s with
  | `Assoc assoc ->
      let version = List.assoc_opt "version" assoc in
      Option.map
        (fun json -> version_of_string (Yojson.Safe.to_string json))
        version
  | _ -> None

let pp ppf t = Fmt.pf ppf "%s" (string_of_t t)
let equal a b = String.equal (string_of_t a) (string_of_t b)
let of_string v = try Ok (t_of_string v) with Failure s -> Error (`Msg s)
let to_json_string d = string_of_t d
let t = Irmin.Type.like ~pp ~equal ~of_string Retirement_data.Types.t
let v ?(version = Retirement_data.latest_version) details = { version; details }
let details t = t.details
let merge' ~old:_ t1 _t2 = Lwt.return (Ok t1)
let merge = Irmin.Merge.(option (v t merge'))

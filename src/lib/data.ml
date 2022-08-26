module Version = struct
  type t = [ `v0 ] [@@deriving irmin]

  let to_string : t -> string = function `v0 -> "v0"

  let of_string = function
    | "v0" -> Ok `v0
    | branch -> Error (branch ^ " is not a valid branch name")

  open Graphql_lwt

  let arg_typ =
    let coerce = function
      | `String s -> of_string s
      | _ -> Error "Invalid input value"
    in
    Schema.Arg.(scalar "VersionInput" ~doc:"" ~coerce)

  let schema_typ =
    let coerce v = `String (to_string v) in
    Schema.scalar ~doc:"The version of the data" "Version" ~coerce

  let latest = `v0

  let of_yojson = function
    | `String v -> of_string v
    | _ -> Error "Versions are serialised as strings"

  let to_yojson v = `String (to_string v)
end

type t = { version : Version.t; json : string } [@@deriving irmin, yojson]

let version t = t.version
let pp ppf t = Fmt.pf ppf "%s" @@ Yojson.Safe.to_string (to_yojson t)
let equal a b = Yojson.Safe.equal (to_yojson a) (to_yojson b)

let of_string s =
  Yojson.Safe.from_string s |> of_yojson |> function
  | Ok v -> Ok v
  | Error s -> Error (`Msg s)

let t = Irmin.Type.like ~pp ~equal ~of_string t
let v json = { version = Version.latest; json = Yojson.Safe.to_string json }
let json t = Yojson.Safe.from_string t.json
let merge' ~old:_ t1 _t2 = Lwt.return (Ok t1)
let merge = Irmin.Merge.(option (v t merge'))

(* GraphQL Custom Types *)
open Graphql_lwt

let schema_typ =
  Schema.(
    obj "Retirement Information"
      ~doc:"A carbon-offsetting retirement data object"
      ~fields:
        [
          field "version" ~typ:(non_null Version.schema_typ) ~args:[]
            ~resolve:(fun _ p -> p.version);
          field "json" ~typ:(non_null string) ~args:[] ~resolve:(fun _ p ->
              p.json);
        ])

let arg_typ =
  Schema.Arg.(
    obj "RetirementInput" ~doc:"Input for retirement data"
      ~fields:
        [
          arg "version" ~doc:"The version of the data"
            ~typ:(non_null Version.arg_typ);
          arg "json" ~typ:(non_null string);
        ]
      ~coerce:(fun version json -> { version; json }))

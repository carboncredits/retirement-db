module Version : sig
  type t = [ `v0 ] [@@deriving irmin]
  (** Versioning of the data stored in the Irmin repository *)

  val latest : t
  (** The latest version *)

  include Irmin_graphql.Server.CUSTOM_TYPE with type t := t
end

type t [@@deriving irmin, yojson]
(** A type for retirement data *)

val v : Yojson.Safe.t -> t
(** A new retirement data, TODO: Type the raw JSON! For now we serialise
    the data and store the raw JSON (except for the version information) *)

val version : t -> Version.t
(** The version of the retirement data *)

val json : t -> Yojson.Safe.t
(** The payload of raw JSON *)

val pp : t Fmt.t
(** A pretty printer for retirement data *)

include Irmin.Contents.S with type t := t
include Irmin_graphql.Server.CUSTOM_TYPE with type t := t

type t = Retirement_data.Types.t
(** A type representing retirement data. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** Parses raw JSON from string into a retirement data. *)

val to_json_string : t -> string
(** Serialises the data to a JSON string *)

val list_to_json_string : t list -> string
(** Converts a list of ts into a JSON string *)

val to_pretty_string : t -> string
(** Like {! to_json_string} but prettier! *)

val pp : t Fmt.t
(** A pretty printer for retirement data *)

val details : t -> Retirement_data.Types.travel_details
(** [details t] returns the travel details associated with the retirement. *)

val version : t -> Retirement_data.Types.version
(** [version t] returns the version of the retirement data. *)

val raw_version : string -> Retirement_data.Types.version option
(** [raw_version json] will try to extract the version information from a raw
    piece of JSON. *)

type finance_details =
  [ `Grant of Retirement_data.Types.grant_details
  | `CostCentre of Retirement_data.Types.cost_centre_details ]

val v :
  ?version:Retirement_data.Types.version ->
  Retirement_data.Types.cambridge_id ->
  finance_details ->
  Retirement_data.Types.travel_details ->
  Retirement_data.Types.offset ->
  t
(** [v ?version details] constructs a new retirement data. If [version] is omitted, the 
    latest version will be used. *)

val dummy_details : t
(** Useful for tests and debugging. *)

include Irmin.Contents.S with type t := t

module Rest : sig
  module Request : sig
    type set = Retirement_data.Types.set_request

    val set_to_json : ?len:int -> set -> string

    type get_hash = Retirement_data.Types.get_hash_request

    val get_hash_to_json : ?len:int -> get_hash -> string

    type get_content = Retirement_data.Types.get_content_request

    val get_content_to_json : ?len:int -> get_content -> string
  end

  module Response : sig
    type set = string Retirement_data.Types.response
    (** The result of setting a new value in the store, returns the hash of the value. *)

    val set_to_json : ?len:int -> set -> string
    val set_of_json : string -> set

    type get_hash = string Retirement_data.Types.response

    val get_hash_to_json : ?len:int -> get_hash -> string
    val get_hash_of_json : string -> get_hash

    type get_content = t Retirement_data.Types.response

    val get_content_to_json : ?len:int -> get_content -> string
    val get_content_of_json : string -> get_content
  end
end

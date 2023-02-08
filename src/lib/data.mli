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

val current_ts : Eio.Time.clock -> string
(** Current time in RFC3339 format. *)

val ts_to_date : string -> int * int * int
(** Convert an RFC3339 timestamp to year, month and date. *)

val timestamp : t -> Ptime.t
(** Get the timestamp of a piece of data *)

val get_path : digest:(t -> string) -> t -> string list
(** Converts a [t] to the path where it will be stored. *)

module Flight_csv : sig
  module Line : sig
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

  val to_csv : t -> Line.t list
  (** [to_csv_by_flight] takes a data entry and converts it to a CSV row
    (comma-separated) for each flight that took place. *)
end

module Finance_csv : sig
  module Line : sig
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

  val to_csv : t -> Line.t option
  (** [to_csv_by_finance] takes a data entry and converts it to a CSV row
    (comma-separated) for financial information. Returns none if there is no
      transaction id. *)
end

val v :
  ?version:Retirement_data.Types.version ->
  ?tx_id:string ->
  timestamp:string ->
  string ->
  Retirement_data.Types.cambridge_id ->
  finance_details ->
  Retirement_data.Types.travel_details ->
  Retirement_data.Types.offset ->
  t
(** [v ?version details] constructs a new retirement data. If [version] is omitted, the
    latest version will be used. *)

val dummy_details : ?tx_id:string -> ?timestamp:string -> Eio.Time.clock -> t
(** Useful for tests and debugging. *)

include Irmin.Contents.S with type t := t

module Rest : sig
  module Request : sig
    type begin_tx = Retirement_data.Types.begin_tx_request

    val begin_tx_to_json : ?len:int -> begin_tx -> string

    type complete_tx = Retirement_data.Types.complete_tx_request

    val complete_tx_to_json : ?len:int -> complete_tx -> string

    type check_tx_status = Retirement_data.Types.check_tx_status_request

    val check_tx_status_to_json : ?len:int -> check_tx_status -> string

    type get_hash = Retirement_data.Types.get_hash_request

    val get_hash_to_json : ?len:int -> get_hash -> string

    type get_content = Retirement_data.Types.get_content_request

    val get_content_to_json : ?len:int -> get_content -> string

    type get_bookers = Retirement_data.Types.get_bookers_request

    val get_bookers_to_json : ?len:int -> get_bookers -> string
  end

  module Response : sig
    type begin_tx = Retirement_data.Types.string_response
    (* Returns the hash of the value. *)

    val begin_tx_to_json : ?len:int -> begin_tx -> string
    val begin_tx_of_json : string -> begin_tx

    type complete_tx = Retirement_data.Types.string_response
    (* Returns the hash of the latest commit in the store *)

    val complete_tx_to_json : ?len:int -> complete_tx -> string
    val complete_tx_of_json : string -> complete_tx

    type check_tx_status = Retirement_data.Types.tx_status_response

    val check_tx_status_to_json : ?len:int -> check_tx_status -> string
    val check_tx_status_of_json : string -> check_tx_status

    type get_hash = Retirement_data.Types.string_response

    val get_hash_to_json : ?len:int -> get_hash -> string
    val get_hash_of_json : string -> get_hash

    type get_content = Retirement_data.Types.t_response

    val get_content_to_json : ?len:int -> get_content -> string
    val get_content_of_json : string -> get_content

    type get_bookers = Retirement_data.Types.t_list_response

    val get_bookers_to_json : ?len:int -> get_bookers -> string
    val get_bookers_of_json : string -> get_bookers
  end
end

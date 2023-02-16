module type Data_store =
  Irmin.S
    with type Schema.Contents.t = Data.t
     and type Schema.Branch.t = string
     and type Schema.Path.t = string list

(* A content-addressed, transaction store. *)
module type S = sig
  type t
  (** A store *)

  type contents
  (** The type of the contents we're storing. *)

  type path = string list
  (** Keys for the contents, which should be unique. *)

  val v : Irmin.config -> t
  (** Construct a new transaction store *)

  val hash_content : contents -> string
  (** Hashes the contents and returns the hash as a hex string. *)

  type tx_error =
    [ `Msg of string
    | `Key_not_unique
    | `Item_exists
    | `Path_exists
    | `Item_does_not_exist ]

  val tx_error_to_string : tx_error -> string

  val begin_transaction :
    ?msg:string ->
    t ->
    clock:Eio.Time.clock ->
    contents ->
    (string, tx_error) result
  (** [begin_transaction t c] starts a transaction for contents [c] and returns
      the hash of the contents if successful. *)

  val complete_transaction :
    ?msg:string ->
    t ->
    clock:Eio.Time.clock ->
    hash:string ->
    tx:string ->
    (string, [ `Msg of string | `Not_pending ]) result
  (** [complete_transaction t ~hash] completes a transaction for [hash] returning the commit hash. *)

  val has_transaction_id : t -> hash:string -> bool option
  (** [has_transaction t ~hash] returns whether or not a [hash] has been successfully
      transacted. It may return [None] is [hash] simply doesn't exist. *)

  val get_transaction_id : t -> hash:string -> string option option
  (** Same idea as {! get_transaction_id}. [Some (Some id)], [Some None] (pending), [None]. *)

  val is_pending : t -> hash:string -> bool
  (** Similar to {! has_transaction} but checks if a [hash] is pending. *)

  val find : t -> hash:string -> contents option
  (** Content-addressed lookup in the store. Doesn't mean it has been transacted. *)

  val lookup_transacted : t -> path -> contents option
  (** Lookup by path in the items that have successfully been transacted. *)

  val lookup_all_transacted : t -> path -> contents list
  (** Find all the contents at a path that have been transacted. *)

  val csv_by_flight : year:int -> month:int -> t -> string
  (** A CSV dump for a particular year and month showing individual flights. *)

  val csv_by_finance : year:int -> month:int -> t -> string
  (** A CSV dump for a particular year and month showing information for finances,
      this means the flights are compressed to just the number but offset information
      in relation to all flights for a given transaction are returned. *)

  val lookup_bookers_transacted :
    booker:string ->
    months:int ->
    current_year:int ->
    current_month:int ->
    t ->
    contents list
  (** [lookup_booker_transacted] will do what {! lookup_all_transacted} does except filter by the
      person who did the booking and can span multiple months. *)

  module Private : sig
    val close : t -> unit
    val dump : t Fmt.t
  end
end

module type Make = functor (Arg : Data_store) ->
  S with type contents = Arg.contents and type path = Arg.path

module type Intf = sig
  module type S = S

  module Make : Make
end

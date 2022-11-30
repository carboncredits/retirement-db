module type Data_store =
  Irmin.S
    with type Schema.Contents.t = Data.t
     and type Schema.Branch.t = string
     and type Schema.Path.t = string list

module type S = sig
  module I : Data_store
  (** The underlying Irmin store *)

  module Sync : Irmin.Sync.S with type db = I.t
  (** Synchronisation functions *)

  val repository : Irmin.config -> I.repo
  (** Get's the underlying Irmin repository *)

  val of_commit : I.repo -> string -> (I.t, [ `Msg of string ]) result
  (** Get's the store associated with a particular hash commit for
        a particular repository. *)

  val of_branch : ?branch:I.branch -> I.repo -> I.t
  (** Get's the store associated with a particular branch for
        a particular repository. *)

  type add_error = [ `Msg of string | I.write_error ]

  val add_error_to_string : add_error -> string

  val add_project_json :
    ?msg:string ->
    clock:Eio.Time.clock ->
    I.t ->
    I.path ->
    string ->
    (string option, add_error) result
  (** Add a project from raw JSON. This will try to parse the JSON into a project
      and may fail to do so, otherwise the failure is to do with persisting the
      data to the store.
      
      Returns the commit hash if successful. *)

  val add_project :
    ?msg:string ->
    clock:Eio.Time.clock ->
    I.t ->
    I.path ->
    Data.t ->
    (string option, add_error) result
  (** Like {! add_project_json} but using a {! Project.t} instead. These can be
        built with {! Project.v}. Returns the commit hash if successful. *)

  val get_project : I.t -> I.path -> Data.t
  (** [get_project store path] gets the project at [path] in [store]. This will raise
      [Not_found] if there is no value at [path]. *)

  val get_project_by_hash :
    I.repo -> string -> (Data.t option, [ `Msg of string ]) result
  (** [get_project_by_hash] is a content-addressed lookup. *)

  val get_all : I.t -> I.path -> Data.t list
  (** [get_all store path] finds all of the stored data items at [path]. *)

  val find_project : I.t -> I.path -> Data.t option
  (** Like {! get_project} only will wrap [Not_found] into an option. *)
end

module type Make = functor (Arg : Data_store) -> S

module type Intf = sig
  module Make : Make
end

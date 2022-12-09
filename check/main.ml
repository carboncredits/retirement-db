module Schema = Irmin.Schema.KV (Irmin.Contents.String)
module Untransacted = Irmin_mem.Make (Schema)
module Transacted = Irmin_mem.Make (Schema)
module Sync = Irmin.Sync.Make (Transacted)

module Global_store = struct
  type t = { hash : string }

  let store = ref []
  let add hash = store := { hash } :: !store
end

let info =
  let t = ref 0L in
  fun () ->
    t := Int64.add 1L !t;
    Untransacted.Info.v !t

let () =
  Eio_main.run @@ fun _ ->
  let config = Irmin_mem.config () in
  let repo = Untransacted.Repo.v config in
  let transacted_repo = Transacted.Repo.v config in
  let content = "My shiny new content" in
  let content_with_no_tx = "Noooooo!" in
  let main = Untransacted.main repo in
  let transacted_main = Transacted.main transacted_repo in
  let will_be_transacted_branch, hash =
    let hash =
      Untransacted.Contents.hash content
      |> Irmin.Type.to_string Untransacted.Hash.t
    in
    let br = Untransacted.of_branch repo hash in
    Untransacted.set_exn ~info br [ hash ] content;
    (br, hash)
  in
  let _untransacted_branch, unhash =
    let hash =
      Untransacted.Contents.hash content_with_no_tx
      |> Irmin.Type.to_string Untransacted.Hash.t
    in
    let br = Untransacted.of_branch repo hash in
    Untransacted.set_exn ~info br [ hash ] content_with_no_tx;
    (br, hash)
  in
  (* Store one of the transactions *)
  Global_store.add hash;
  (* That returned successfully and now we merhe that branch into
     the untransacted main. *)
  let () =
    Result.get_ok
    @@ Untransacted.merge_into ~into:main ~info will_be_transacted_branch
  in
  (* Now we sync untransacted main to transacted main *)
  let untransacted_remote = Irmin.remote_store (module Untransacted) main in
  let _status = Sync.pull_exn transacted_main untransacted_remote `Set in
  Fmt.pr "\n<><><> Untransacted Store <><><>\n";
  Fmt.pr "Do we have %s? %a\n" hash
    Fmt.(option ~none:(Fmt.(const string) "NONE") string)
    (Untransacted.Contents.of_hash repo
       (Result.get_ok @@ Irmin.Type.of_string Transacted.Hash.t hash));
  Fmt.pr "Do we have %s? %a\n" unhash
    Fmt.(option ~none:(Fmt.(const string) "NONE") string)
    (Untransacted.Contents.of_hash repo
       (Result.get_ok @@ Irmin.Type.of_string Transacted.Hash.t unhash));
  Fmt.pr "\n<><><> Transaction Store <><><>\n";
  Fmt.pr "Do we have %s? %a\n" hash
    Fmt.(option ~none:(Fmt.(const string) "NONE") string)
    (Transacted.Contents.of_hash transacted_repo
       (Result.get_ok @@ Irmin.Type.of_string Transacted.Hash.t hash));
  Fmt.pr "Do we have %s? %a\n" unhash
    Fmt.(option ~none:(Fmt.(const string) "NONE") string)
    (Transacted.Contents.of_hash transacted_repo
       (Result.get_ok @@ Irmin.Type.of_string Transacted.Hash.t unhash))

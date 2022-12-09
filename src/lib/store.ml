open Eio

module type Project_store =
  Irmin.S
    with type Schema.Contents.t = Data.t
     and type Schema.Branch.t = string
     and type Schema.Path.t = string list

module Make (S : Project_store) = struct
  module I = S
  module Sync = Irmin.Sync.Make (I)

  type add_error = [ `Msg of string | S.write_error ]

  let add_error_to_string = function
    | `Msg m -> m
    | #S.write_error as e -> Irmin.Type.to_string S.write_error_t e

  let repository config = S.Repo.v config

  let of_commit repo hash =
    match Irmin.Type.of_string I.Hash.t hash with
    | Error e -> Error e
    | Ok h -> (
        let commit = I.Commit.of_hash repo h in
        match commit with
        | Some c -> Ok (I.of_commit c)
        | None -> Error (`Msg ("No store found for commit " ^ hash)))

  let of_branch ?branch repo =
    match branch with None -> I.main repo | Some b -> I.of_branch repo b

  let info ~clock message () =
    S.Info.v ~author:"project-server" ~message (Time.now clock |> Int64.of_float)

  let default_message path = function
    | Some msg -> msg
    | None -> "Add project to " ^ String.concat "/" path

  let add_new_project ?msg ~clock store path p =
    S.test_set_and_get
      ~info:(info ~clock (default_message path msg))
      store path ~test:None ~set:(Some p)

  let commit_hash = function
    | Ok (Some commit) ->
        Ok (Some (S.Commit.hash commit |> Irmin.Type.to_string I.Hash.t))
    | Ok None -> Ok None
    | Error e -> Error e

  let add_project_json ?msg ~clock store path json =
    match Data.of_string json with
    | Error _ as e -> (e :> (string option, add_error) result)
    | Ok p ->
        (add_new_project ?msg ~clock store path p |> commit_hash
          :> (string option, add_error) result)

  let add_project ?msg ~clock store path project =
    (add_new_project ?msg ~clock store path project |> commit_hash
      :> (string option, add_error) result)

  let get_project s path = I.get s path

  let get_project_by_hash repo hash =
    match Irmin.Type.of_string I.Hash.t hash with
    | Error e -> Error e
    | Ok h -> Ok (I.Contents.of_hash repo h)

  let get_all s path =
    let items = I.list s path in
    let items =
      List.map
        (fun (s, t) ->
          let t' = S.Tree.to_concrete t in
          (s, t'))
        items
    in
    List.fold_left
      (fun acc -> function _, `Contents (c, _) -> c :: acc | _ -> acc)
      [] items

  let find_project s path = I.find s path
  let content_hash t = I.Contents.hash t |> Irmin.Type.to_string I.Hash.t
end

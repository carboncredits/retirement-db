module type Project_store =
  Irmin.S
    with type Schema.Contents.t = Data.t
     and type Schema.Branch.t = string
     and type Schema.Path.t = string list

let await = Lwt_eio.Promise.await_lwt

module Make (S : Project_store) = struct
  module I = S
  module Sync = Irmin.Sync.Make (I)

  type add_error = [ `Msg of string | S.write_error ]

  let add_error_to_string = function
    | `Msg m -> m
    | #S.write_error as e -> Irmin.Type.to_string S.write_error_t e

  let repository config = S.Repo.v config |> await

  let of_commit repo hash =
    match Irmin.Type.of_string I.Hash.t hash with
    | Error e -> Error e
    | Ok h -> (
        let commit = await @@ I.Commit.of_hash repo h in
        match commit with
        | Some c -> Ok (await @@ I.of_commit c)
        | None -> Error (`Msg ("No store found for commit " ^ hash)))

  let of_branch ?branch repo =
    let p =
      match branch with None -> I.main repo | Some b -> I.of_branch repo b
    in
    await p

  let info message () =
    S.Info.v ~author:"project-server" ~message
      (Unix.gettimeofday () |> Int64.of_float)

  let default_message path = function
    | Some msg -> msg
    | None -> "Add project to " ^ String.concat "/" path

  let add_new_project_lwt ?msg store path p =
    S.test_set_and_get
      ~info:(info (default_message path msg))
      store path ~test:None ~set:(Some p)

  let commit_hash = function
    | Ok (Some commit) ->
        Ok (Some (S.Commit.hash commit |> Irmin.Type.to_string I.Hash.t))
    | Ok None -> Ok None
    | Error e -> Error e

  let add_project_json ?msg store path json =
    match Data.of_string json with
    | Error _ as e -> (e :> (string option, add_error) result)
    | Ok p ->
        (Lwt_eio.Promise.await_lwt (add_new_project_lwt ?msg store path p)
         |> commit_hash
          :> (string option, add_error) result)

  let add_project ?msg store path project =
    (Lwt_eio.Promise.await_lwt @@ add_new_project_lwt ?msg store path project
     |> commit_hash
      :> (string option, add_error) result)

  let get_project_lwt s path = I.get s path
  let get_project s path = get_project_lwt s path |> await

  let get_project_by_hash repo hash =
    match Irmin.Type.of_string I.Hash.t hash with
    | Error e -> Error e
    | Ok h -> Ok (await @@ I.Contents.of_hash repo h)

  let get_all_lwt s path =
    let open Lwt.Syntax in
    let* items = I.list s path in
    let+ items =
      Lwt_list.map_s
        (fun (s, t) ->
          let+ t' = S.Tree.to_concrete t in
          (s, t'))
        items
    in
    List.fold_left
      (fun acc -> function _, `Contents (c, _) -> c :: acc | _ -> acc)
      [] items

  let get_all s path = get_all_lwt s path |> await
  let find_project_lwt s path = I.find s path
  let find_project s path = find_project_lwt s path |> await
end

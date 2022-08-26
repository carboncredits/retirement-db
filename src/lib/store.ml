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

  let repository config = S.Repo.v config |> await

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

  let add_project_lwt ?msg store path p =
    (S.set ~info:(info (default_message path msg)) store path p
      :> (unit, add_error) Lwt_result.t)

  let add_project_json ?msg store path json =
    match Data.of_yojson json with
    | Error s -> Error (`Msg s)
    | Ok p -> Lwt_eio.Promise.await_lwt (add_project_lwt ?msg store path p)

  let add_project ?msg store path project =
    Lwt_eio.Promise.await_lwt @@ add_project_lwt ?msg store path project

  let get_project_lwt s path = I.get s path
  let get_project s path = get_project_lwt s path |> await
  let find_project_lwt s path = I.find s path
  let find_project s path = find_project_lwt s path |> await
end

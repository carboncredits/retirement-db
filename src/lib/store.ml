open Eio
include Store_intf

module Make (S : Data_store) = struct
  module I = S
  module Sync = Irmin.Sync.Make (I)

  type t = { transacted : S.Repo.t }
  type path = S.path
  type contents = S.contents

  let v config =
    let transacted = S.Repo.v config in
    { transacted }

  let hash_content v =
    let h = S.Contents.hash v in
    Irmin.Type.to_string S.Hash.t h

  type tx_error =
    [ `Msg of string
    | `Key_not_unique
    | `Item_exists
    | `Path_exists
    | `Item_does_not_exist ]

  let tx_error_to_string = function
    | `Msg s -> s
    | `Key_not_unique -> "Key not unique!"
    | `Item_exists -> "Item exists!"
    | `Item_does_not_exist -> "Item doesn't exist!"
    | `Path_exists -> "Path exists!"

  let info ?msg clock = S.Info.v ?message:msg (Time.now clock |> Int64.of_float)

  let test_and_set ~test ~info tx_b contents path =
    match S.test_set_and_get ~info tx_b ~test ~set:(Some contents) path with
    | Ok c -> Ok c
    | Error (`Test_was (Some _)) -> Error `Item_exists
    | Error (`Conflict _) -> Error `Path_exists
    | Error (`Test_was None) -> Error `Item_does_not_exist
    | Error e ->
        ignore (failwith (Option.is_none test |> string_of_bool));
        Error (`Msg (Irmin.Type.to_string S.write_error_t e))

  let begin_transaction ?msg t ~clock (c : Data.t) =
    let info () = info ?msg clock in
    let h = S.Contents.hash c in
    if Option.is_some @@ S.Contents.of_hash t.transacted h then
      Error `Item_exists
    else
      let hash_string = Irmin.Type.to_string S.Hash.t h in
      let path = Data.get_path ~digest:hash_content c in
      let tx_b = S.of_branch t.transacted hash_string in
      Result.map
        (fun _ -> Irmin.Type.to_string S.Hash.t h)
        (test_and_set ~test:None ~info tx_b c path)

  let complete_transaction ?msg t ~clock ~hash ~tx =
    if not (S.Branch.mem t.transacted hash) then Error `Not_pending
    else
      let info () = info ?msg clock in
      let main = S.main t.transacted in
      let tx_b = S.of_branch t.transacted hash in
      (* Gauranteed to find value *)
      match
        S.Contents.of_hash t.transacted
          (Result.get_ok @@ Irmin.Type.of_string S.Hash.t hash)
      with
      | None -> Error `Not_pending
      | Some old -> (
          (* Update value with transaction and merge branch into main. *)
          let year, month, _ = old.ts |> Data.ts_to_date in
          let updated = { old with tx_id = Some tx } in
          match
            test_and_set ~test:(Some old) ~info tx_b updated
              [ string_of_int year; string_of_int month; hash ]
          with
          | Error `Item_exists -> Error (`Msg "Item exists")
          | Error `Item_does_not_exist -> Error (`Msg "Item does not exists")
          | Error `Path_exists -> Error (`Msg "Path exists")
          | Error (`Msg _) as e -> e
          | Ok None -> Error (`Msg ("Store didn't update " ^ hash))
          | Ok (Some c) -> (
              match S.merge_into ~into:main ~info tx_b with
              | Error (`Conflict e) -> Error (`Msg e)
              | Ok () ->
                  (* Remove the transaction branch *)
                  S.Branch.remove t.transacted hash;
                  Ok (S.Commit.hash c |> Irmin.Type.to_string S.Hash.t)))

  let get_transaction_id t ~hash =
    let h = Result.get_ok @@ Irmin.Type.of_string S.Hash.t hash in
    match S.Contents.of_hash t.transacted h with
    | None -> None
    | Some c -> (
        let path = Data.get_path ~digest:hash_content c in
        match S.find (S.main t.transacted) path with
        | Some t -> Some t.tx_id
        | None -> Some None)

  let has_transaction_id t ~hash =
    match get_transaction_id t ~hash with
    | Some (Some _) -> Some true
    | Some None -> Some false
    | None -> None

  let is_pending t ~hash =
    match has_transaction_id t ~hash with Some false -> true | _ -> false

  let find t ~hash =
    let hash = Result.get_ok @@ Irmin.Type.of_string S.Hash.t hash in
    S.Contents.of_hash t.transacted hash

  let lookup_transacted t path = S.find (S.main t.transacted) path

  let lookup_all_transacted t path =
    let items = S.list (S.main t.transacted) path in
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
    |> List.stable_sort (fun a b ->
           Ptime.compare (Data.timestamp a) (Data.timestamp b))

  let get_all t =
    let root = S.tree (S.main t.transacted) in
    S.Tree.fold ~contents:(fun _ c acc -> c :: acc) root []

  let previous_year_months ~months current_year current_month =
    let months =
      List.init months (fun i ->
          let d = current_month - 1 - i in
          if d < 0 then (current_year + ((d / 12) - 1), 12 + (d mod 12))
          else (current_year, d))
    in
    List.map
      (fun (year, month) -> [ string_of_int year; string_of_int (month + 1) ])
      months

  let lookup_bookers_transacted ~booker ~months ~current_year ~current_month t =
    let months = previous_year_months ~months current_year current_month in
    let vs = List.map (lookup_all_transacted t) months |> List.concat in
    List.filter
      (fun v -> String.equal v.Retirement_data.Types.booker_crsid booker)
      vs
    |> List.stable_sort (fun a b ->
           Ptime.compare (Data.timestamp a) (Data.timestamp b))

  let csv_by_flight ~year ~month store =
    let items =
      lookup_all_transacted store [ string_of_int year; string_of_int month ]
    in
    let csvs = List.map Data.Flight_csv.to_csv items |> List.concat in
    let header = Data.Flight_csv.Line.csv_header in
    let buffer = Buffer.create 128 in
    Buffer.add_string buffer (String.concat "," header ^ "\n");
    List.iter
      (fun v ->
        Buffer.add_string buffer
        @@ (String.concat "," @@ Data.Flight_csv.Line.row_of_t v)
        ^ "\n")
      csvs;
    Buffer.contents buffer

  let csv_by_finance ~year ~month store =
    let items =
      lookup_all_transacted store [ string_of_int year; string_of_int month ]
    in
    let csvs = List.filter_map Data.Finance_csv.to_csv items in
    let header = Data.Finance_csv.Line.csv_header in
    let buffer = Buffer.create 128 in
    Buffer.add_string buffer (String.concat "," header ^ "\n");
    List.iter
      (fun v ->
        Buffer.add_string buffer
        @@ (String.concat "," @@ Data.Finance_csv.Line.row_of_t v)
        ^ "\n")
      csvs;
    Buffer.contents buffer

  module Private = struct
    let close t = S.Repo.close t.transacted

    let dump ppf t =
      let items = get_all t in
      Fmt.pf ppf "%a" Fmt.(list (Irmin.Type.pp Data.t)) items
  end
end

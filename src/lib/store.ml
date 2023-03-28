open Lwt.Syntax
include Store_intf

module Make (S : Data_store) = struct
  module I = S
  module Sync = Irmin.Sync.Make (I)

  type t = { transacted : S.Repo.t }
  type path = S.path
  type contents = S.contents

  let v config =
    let+ transacted = S.Repo.v config in
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

  let info ?msg clock =
    S.Info.v ?message:msg (Data.Time.now clock |> Int64.of_float)

  let test_and_set ~test ~info tx_b contents path =
    let+ res = S.test_set_and_get ~info tx_b ~test ~set:(Some contents) path in
    match res with
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
    let* hash = S.Contents.of_hash t.transacted h in
    if Option.is_some hash then Lwt.return @@ Error `Item_exists
    else
      let hash_string = Irmin.Type.to_string S.Hash.t h in
      let path = Data.get_path ~digest:hash_content c in
      let* tx_b = S.of_branch t.transacted hash_string in
      Lwt_result.map
        (fun _ -> Irmin.Type.to_string S.Hash.t h)
        (test_and_set ~test:None ~info tx_b c path)

  let complete_transaction ?msg t ~clock ~hash ~tx =
    let* mem = S.Branch.mem t.transacted hash in
    if not mem then Lwt.return @@ Error `Not_pending
    else
      let info () = info ?msg clock in
      let* main = S.main t.transacted in
      let* tx_b = S.of_branch t.transacted hash in
      (* Gauranteed to find value *)
      let* value =
        S.Contents.of_hash t.transacted
          (Result.get_ok @@ Irmin.Type.of_string S.Hash.t hash)
      in
      match value with
      | None -> Lwt.return @@ Error `Not_pending
      | Some old -> (
          (* Update value with transaction and merge branch into main. *)
          let year, month, _ = old.ts |> Data.ts_to_date in
          let updated = { old with tx_id = Some tx } in
          let* res =
            test_and_set ~test:(Some old) ~info tx_b updated
              [ string_of_int year; string_of_int month; hash ]
          in
          match res with
          | Error `Item_exists -> Lwt.return @@ Error (`Msg "Item exists")
          | Error `Item_does_not_exist ->
              Lwt.return @@ Error (`Msg "Item does not exists")
          | Error `Path_exists -> Lwt.return @@ Error (`Msg "Path exists")
          | Error (`Msg _) as e -> Lwt.return e
          | Ok None ->
              Lwt.return @@ Error (`Msg ("Store didn't update " ^ hash))
          | Ok (Some c) -> (
              let* merge = S.merge_into ~into:main ~info tx_b in
              match merge with
              | Error (`Conflict e) -> Lwt.return @@ Error (`Msg e)
              | Ok () ->
                  (* Remove the transaction branch *)
                  let+ () = S.Branch.remove t.transacted hash in
                  Ok (S.Commit.hash c |> Irmin.Type.to_string S.Hash.t)))

  let get_transaction_id t ~hash =
    let h = Result.get_ok @@ Irmin.Type.of_string S.Hash.t hash in
    let* hash = S.Contents.of_hash t.transacted h in
    match hash with
    | None -> Lwt.return_none
    | Some c -> (
        let path = Data.get_path ~digest:hash_content c in
        let* main = S.main t.transacted in
        let+ f = S.find main path in
        match f with Some t -> Some t.tx_id | None -> Some None)

  let has_transaction_id t ~hash =
    let+ id = get_transaction_id t ~hash in
    match id with
    | Some (Some _) -> Some true
    | Some None -> Some false
    | None -> None

  let is_pending t ~hash =
    let+ id = has_transaction_id t ~hash in
    match id with Some false -> true | _ -> false

  let find t ~hash =
    let hash = Result.get_ok @@ Irmin.Type.of_string S.Hash.t hash in
    S.Contents.of_hash t.transacted hash

  let lookup_transacted t path =
    let* main = S.main t.transacted in
    S.find main path

  let lookup_all_transacted t path =
    let* main = S.main t.transacted in
    let* items = S.list main path in
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
    |> List.stable_sort (fun a b ->
           Ptime.compare (Data.timestamp a) (Data.timestamp b))

  let get_all t =
    let* main = S.main t.transacted in
    let* root = S.tree main in
    S.Tree.fold ~contents:(fun _ c acc -> Lwt.return @@ (c :: acc)) root []

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
    let+ vs =
      Lwt_list.map_s (lookup_all_transacted t) months |> Lwt.map List.concat
    in
    List.filter
      (fun v -> String.equal v.Retirement_data.Types.booker_crsid booker)
      vs
    |> List.stable_sort (fun a b ->
           Ptime.compare (Data.timestamp a) (Data.timestamp b))

  let csv_by_flight ~year ~month store =
    let+ items =
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
    let+ items =
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
      let+ items = get_all t in
      Fmt.pf ppf "%a" Fmt.(list (Irmin.Type.pp Data.t)) items
  end
end

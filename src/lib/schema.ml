include Irmin.Schema.KV (Data)
module Md = Multihash_digestif

module Hash = struct
  type t = string Md.t

  let hash = `Sha2_256

  external get_64 : string -> int -> int64 = "%caml_string_get64u"
  external swap64 : int64 -> int64 = "%bswap_int64"

  let get_64_little_endian str idx =
    if Sys.big_endian then swap64 (get_64 str idx) else get_64 str idx

  let short_hash c = Int64.to_int (get_64_little_endian (Md.write c) 0)

  let short_hash_substring bigstring ~off =
    Int64.to_int (Bigstringaf.get_int64_le bigstring off)

  let hash_size =
    Md.of_cstruct hash Cstruct.empty
    |> Result.get_ok |> Md.write |> Cstruct.length

  let code x =
    match x with
    | '0' .. '9' -> Char.code x - Char.code '0'
    | 'A' .. 'F' -> Char.code x - Char.code 'A' + 10
    | 'a' .. 'f' -> Char.code x - Char.code 'a' + 10
    | _ -> invalid_arg (Fmt.str "of_hex: %02X" (Char.code x))

  let decode chr1 chr2 = Char.chr ((code chr1 lsl 4) lor code chr2)

  let of_hex digest_size hex =
    let offset = ref 0 in
    let rec go have_first idx =
      if !offset + idx >= String.length hex then '\x00'
      else
        match hex.[!offset + idx] with
        | ' ' | '\t' | '\r' | '\n' ->
            incr offset;
            go have_first idx
        | chr2 when have_first -> chr2
        | chr1 ->
            incr offset;
            let chr2 = go true idx in
            if chr2 <> '\x00' then decode chr1 chr2
            else invalid_arg "of_hex: odd number of hex characters"
    in
    String.init digest_size (go false)

  let of_hex s =
    match of_hex hash_size s |> Md.read_string with
    | Error (`Msg _) as e -> e
    | Ok _t as v -> v

  let to_hex hash =
    let res = Bytes.create (hash_size * 2) in
    let chr x =
      match x with
      | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 -> Char.chr (48 + x)
      | _ -> Char.chr (97 + (x - 10))
    in
    for i = 0 to hash_size - 1 do
      let v = Char.code hash.[i] in
      Bytes.unsafe_set res (i * 2) (chr (v lsr 4));
      Bytes.unsafe_set res ((i * 2) + 1) (chr (v land 0x0F))
    done;
    Bytes.unsafe_to_string res

  let pp_hex ppf x = Fmt.string ppf (Md.write x |> to_hex)
  let read v = Result.get_ok (Md.read_string v)
  let write v = Md.write v

  let t =
    let open Irmin in
    Type.map ~pp:pp_hex ~of_string:of_hex
      Type.(string_of (`Fixed hash_size))
      read write

  let hash (f : (string -> unit) -> unit) : t =
    Md.iter_string hash f |> Result.get_ok

  let to_raw_string = write
  let unsafe_of_raw_string = read
end

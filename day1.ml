open Core

let rec consec_digit_sum' accum digits =
  match digits with
  | [] | [_] -> accum
  | hd :: hd' :: tl ->
    let score = if hd = hd' then hd else 0 in
    consec_digit_sum' (accum + score) (hd' :: tl)

let consec_digit_sum digits_str =
  let digit_vals = List.filter_map (String.to_list digits_str) ~f:Char.get_digit in
  let wrapped_vals =
    match List.last digit_vals with
    | Some d -> d :: digit_vals
    | None -> []
  in
  consec_digit_sum' 0 wrapped_vals

let rec halfway_consec_digit_sum' digits accum pos offset =
  let list_len = List.length digits in
  if (pos >= list_len) then accum
  else
    let halfway_offset_digit = List.nth digits ((pos + offset) % list_len) in
    let curr_digit = List.nth digits pos in
    match Option.both halfway_offset_digit curr_digit with
    | Some (halfway_offset, curr) ->
      let score = if halfway_offset = curr then curr else 0 in
      halfway_consec_digit_sum' digits (accum + score) (pos + 1) offset
    | None -> assert false

let () =
  In_channel.read_all "captcha.txt"
  |> String.strip
  |> consec_digit_sum
  |> print_int
  |> print_newline

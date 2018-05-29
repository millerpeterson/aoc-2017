open Core

let rec consec_digit_sum' accum digits =
  match digits with
  | [] | [_] -> accum
  | hd :: hd' :: tl ->
    let score = if hd = hd' then hd else 0 in
    consec_digit_sum' (accum + score) (hd' :: tl)

let digits_from_str digits_str =
  List.filter_map (String.to_list digits_str) ~f:Char.get_digit

let consec_digit_sum digits_str =
  let digits = digits_from_str digits_str in
  let wrapped_vals =
    match List.last digits with
    | Some d -> d :: digits
    | None -> []
  in
  consec_digit_sum' 0 wrapped_vals

let rec halfway_consec_digit_sum' digits accum pos offset =
  let list_len = List.length digits in
  if (pos >= list_len) then
    accum
  else
    let halfway_offset_digit = List.nth digits ((pos + offset) % list_len) in
    let curr_digit = List.nth digits pos in
    match Option.both halfway_offset_digit curr_digit with
    | Some (halfway_offset, curr) ->
      let score = if halfway_offset = curr then curr else 0 in
      halfway_consec_digit_sum' digits (accum + score) (pos + 1) offset
    | None -> failwith "Invalid digits provided as input"

let halfway_consec_digit_sum digits_str =
  let digits = digits_from_str digits_str in
  let offset = (List.length digits) / 2 in
  halfway_consec_digit_sum' digits 0 0 offset

let captcha_input =
  In_channel.read_all "captcha.txt"
  |> String.strip

let () =
  (* consec_digit_sum tests *)
  assert (consec_digit_sum "1122" = 3);
  assert (consec_digit_sum "1111" = 4);
  assert (consec_digit_sum "1234" = 0);
  assert (consec_digit_sum "91212129" = 9);
  assert (consec_digit_sum captcha_input = 1251);

  (* halfway_consec_digit_sum tests *)
  assert (halfway_consec_digit_sum "1212" = 6);
  assert (halfway_consec_digit_sum "1221" = 0);
  assert (halfway_consec_digit_sum "123425" = 4);
  assert (halfway_consec_digit_sum "123123" = 12);
  assert (halfway_consec_digit_sum "12131415" = 4);
  assert (halfway_consec_digit_sum captcha_input = 1244);

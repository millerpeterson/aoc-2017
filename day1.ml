open Core.Std

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

let () =
  In_channel.read_all "captcha.txt"
  |> String.strip
  |> consec_digit_sum
  |> print_int
  |> print_newline

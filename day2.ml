open Core

let row_difference row_values =
  let min = List.min_elt row_values Int.compare in
  let max = List.max_elt row_values Int.compare in
  match Option.both min max with
  | Some (min, max) -> max - min
  | None -> 0

let line_to_row_values line =
  let space_split = String.split (String.strip line) ~on:'\t' in
  List.filter_map space_split ~f:int_of_string_opt

let spreadsheet_checksum spreadsheet_filename =
  let line_differences = In_channel.read_all spreadsheet_filename
                         |> String.split_lines
                         |> List.map ~f:line_to_row_values
                         |> List.map ~f:row_difference
  in
  let checksum = List.reduce line_differences ~f:(+) in
  match checksum with
  | Some c -> c
  | None -> failwith "Invalid spreadsheet input"

let () =
  assert (spreadsheet_checksum "spreadsheet.txt" = 39126)

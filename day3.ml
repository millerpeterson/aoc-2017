open Core

(* the center point of the spiral *)
let origin = (0, 0)

let add_pt u v =
  let (ux, uy) = u in
  let (vx, vy) = v in
  (ux + vx, uy + vy)

let mult_pt c v =
  let (vx, vy) = v in
  (c * vx, c * vy)

let spiral_dir_order = [(0, 1); (* 1. up *)
                        (-1, 0); (* 2. left *)
                        (0, -1);  (* 3. down *)
                        (1, 0)]  (* 4. right *)

let spiral_path_origin level =
  mult_pt (1 + level) (1, -1)

let n_times e n =
  List.fold (List.range 0 n)
    ~init:[]
    ~f:(fun l _ -> List.cons e l)

let spiral_move_seq level =
  let dir_continue = (level + 1) * 2 in
  List.map spiral_dir_order ~f:(fun dir -> n_times dir dir_continue)
  |> List.concat

let spiral_path_pos level n =
  let origin = spiral_path_origin level in
  let moves = List.take (spiral_move_seq level) n in
  List.fold moves ~init:origin ~f:add_pt

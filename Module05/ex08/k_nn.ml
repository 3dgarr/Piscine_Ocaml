type radar = float array * string

let eu_dist (p1 : float array) (p2 : float array) : float =
  if Array.length p1 <> Array.length p2 then
    invalid_arg "Vectors must have the same length"
  else
    let rec aux acc i =
      if i < 0 then acc
      else aux (acc +. (p1.(i) -. p2.(i)) ** 2.0) (i - 1)
    in
  sqrt (aux 0.0 (Array.length p1 - 1))

(* Compute distances *)
let distances training vector =
  List.map (fun (v, cls) -> (eu_dist vector v, cls)) training

(* Take first k elements *)
let k_nearest k dist_list =
  let sorted = List.sort (fun (d1, _) (d2, _) -> compare d1 d2) dist_list in
  let rec take acc n = function
    | [] -> List.rev acc
    | h :: t -> if n = 0 then List.rev acc else take (h :: acc) (n - 1) t
  in
  take [] k sorted

(* Find most frequent label *)
let most_frequent labels =
  let counts = Hashtbl.create 10 in
  List.iter (fun cls ->
    let c = try Hashtbl.find counts cls with Not_found -> 0 in
    Hashtbl.replace counts cls (c + 1)
  ) labels;
  let max_count = ref (-1) in
  let max_cls = ref "" in
  Hashtbl.iter (fun cls c ->
    if c > !max_count then (max_count := c; max_cls := cls)
  ) counts;
  !max_cls

(* K-NN function *)
let k_nn (training : radar list) (k : int) ((vector : float array), _ : radar) : string =
  let dist_list = distances training vector in
  let nearest_k = k_nearest k dist_list in
  let labels = List.map snd nearest_k in
  most_frequent labels

(* ------------------- Tests ------------------- *)
let () =
  let training = [
    ([|0.0; 0.0|], "A");
    ([|0.0; 1.0|], "B");
    ([|1.0; 0.0|], "C");
    ([|1.0; 1.0|], "D");
  ] in

  let test1 = ([|0.1; 0.1|], "?") in
  let test2 = ([|0.9; 0.1|], "?") in
  let test3 = ([|0.2; 0.9|], "?") in
  let test4 = ([|0.8; 0.8|], "?") in

  (* K=1 is same as 1-NN *)
  assert (k_nn training 1 test1 = "A");
  assert (k_nn training 1 test2 = "C");
  assert (k_nn training 1 test3 = "B");
  assert (k_nn training 1 test4 = "D");

  (* K=2, check majority vote *)
  assert (k_nn training 2 test1 = "A" || k_nn training 2 test1 = "C");
  assert (k_nn training 2 test4 = "D" || k_nn training 2 test4 = "B")

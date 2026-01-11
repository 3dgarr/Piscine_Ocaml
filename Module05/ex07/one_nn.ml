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

let one_nn (training : radar list) ((vector : float array), _ : radar) : string =
  match training with
  | [] -> failwith "Training set is empty"
  | (v, cls) :: rest ->
      let best_dist = ref (eu_dist vector v) in
      let best_cls = ref cls in
      List.iter (fun (v2, cls2) ->
        let d = eu_dist vector v2 in
        if d < !best_dist then (
          best_dist := d;
          best_cls := cls2
        )
      ) rest;
      !best_cls


let () =
  (* Training set *)
  let training = [
    ([|0.0; 0.0|], "A");   (* bottom-left corner *)
    ([|0.0; 1.0|], "B");   (* top-left corner *)
    ([|1.0; 0.0|], "C");   (* bottom-right corner *)
    ([|1.0; 1.0|], "D");   (* top-right corner *)
  ] in

  (* Test radars *)
  let test1 = ([|0.1; 0.1|], "?") in  (* closest to A *)
  let test2 = ([|0.9; 0.1|], "?") in  (* closest to C *)
  let test3 = ([|0.2; 0.9|], "?") in  (* closest to B *)
  let test4 = ([|0.8; 0.8|], "?") in  (* closest to D *)
  let test5 = ([|0.0; 0.4|], "?") in  (* closest to A *)
  let test6 = ([|0.5; 0.0|], "?") in  (* closest to A *)

  (* Assertions *)
  assert (one_nn training test1 = "A");
  assert (one_nn training test2 = "C");
  assert (one_nn training test3 = "B");
  assert (one_nn training test4 = "D");
  assert (one_nn training test5 = "A");
  assert (one_nn training test6 = "A");

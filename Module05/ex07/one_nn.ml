type radar = float array * string

let euclidean_distance (v1 : float array) (v2 : float array) : float =
  let sum = ref 0.0 in
  for i = 0 to Array.length v1 - 1 do
    let diff = v1.(i) -. v2.(i) in
    sum := !sum +. (diff *. diff)
  done; 
  sqrt !sum

let one_nn (training : radar list) ((vector : float array), _ : radar) : string =
  match training with
  | [] -> failwith "Training set is empty"
  | (v, cls) :: rest ->
      let best_dist = ref (euclidean_distance vector v) in
      let best_cls = ref cls in
      List.iter
        (fun (v2, cls2) ->
          let d = euclidean_distance vector v2 in
          if d < !best_dist then (
            best_dist := d;
            best_cls := cls2))
        rest;
      !best_cls

let () =
  let training = [
    ([|1.0; 0.5|], "g");
    ([|2.0; 1.5|], "b");
    ([|0.9; 0.4|], "g")
  ] in
  let radar_to_guess = ([|1.1; 0.6|], "?") in
  let predicted = one_nn training radar_to_guess in
  Printf.printf "Predicted class: %s\n" predicted



let eu_dist (p1 : float array) (p2 : float array) : float =
  if Array.length p1 <> Array.length p2 then
    invalid_arg "Vectors must have the same length"
  else
    let rec aux acc i =
      if i < 0 then acc
      else aux (acc +. (p1.(i) -. p2.(i)) ** 2.0) (i - 1)
    in
  sqrt (aux 0.0 (Array.length p1 - 1))

let () =
  try
    let p1 = [|1.0; 2.0; 3.0|] in
    let p2 = [|4.0; 5.0; 6.0|] in
    let d = eu_dist p1 p2 in
    Printf.printf "The Euclidean distance between the two points is %.2f\n" d;

    let p3 = [|1.0; 2.0|] in
    let p4 = [|4.0; 5.0|] in
    let d2 = eu_dist p3 p4 in
    Printf.printf "The Euclidean distance between the two points is %.2f\n" d2;
    
    let p5 = [|1.0; 2.0; 3.0; 4.0|] in
    let p6 = [|4.0; 5.0; 6.0; 7.0|] in
    let d3 = eu_dist p5 p6 in
    Printf.printf "The Euclidean distance between the two points is %.2f\n" d3;
    
    let p7 = [|1.0; 2.0; 3.0; 4.0; 5.0|] in
    let p8 = [|5.0; 4.0; 3.0; 2.0; 1.0|] in
    let d4 = eu_dist p7 p8 in
    Printf.printf "The Euclidean distance between the two points is %.2f\n" d4;
    
    let p11 = [|1.0; -2.0; 3.0|] in
    let p12 = [|4.0; 5.0; -6.0|] in
    let d5 = eu_dist p11 p12 in
    Printf.printf "The Euclidean distance between the two points is %.2f\n" d5;
    
    let p9 = [|1.0; 2.0; 3.0|] in
    let p10 = [|4.0; 5.0|] in
    let d6 = eu_dist p9 p10 in
    Printf.printf "The Euclidean distance between the two points is %.2f\n" d6;


with Invalid_argument msg ->
  Printf.eprintf "Error: %s\n" msg;
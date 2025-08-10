(* 

You will write a function named sum that takes two floating-point numbers and adds
them together. Yes, that’s it.
Your function’s type will be float -> float -> float.

*)

let sum : float -> float -> float = fun x y -> x +. y


let () =
  let a = 3.0 in
  let b = 4.0 in
  let c = sum a b in
  Printf.printf "The sum of %.2f and %.2f is %.2f\n" a b c;

  let d = 5.0 in
  let e = 6.0 in
  let f = sum d e in
  Printf.printf "The sum of %.2f and %.2f is %.2f\n" d e f;

  let g = 7.0 in
  let h = 8.0 in
  let i = sum g h in
  Printf.printf "The sum of %.2f and %.2f is %.2f\n" g h i;

  let j = 9.0 in
  let k = 10.0 in
  let l = sum j k in
  Printf.printf "The sum of %.2f and %.2f is %.2f\n" j k l;
  (* extra bad cases please *)

  let m = 1.0 in
  let n = 2.0 in
  let o = sum m n in
  Printf.printf "The sum of %.2f and %.2f is %.2f\n" m n o;


  let p = 0.0 in
  let q = 0.0 in
  let r = sum p q in
  Printf.printf "The sum of %.2f and %.2f is %.2f\n" p q r;

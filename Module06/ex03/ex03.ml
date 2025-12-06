(* --------------------------------------------------------- *)
(* Module06/ex03/ex03.ml                                     *)
(*                                                           *)
(* Fixed-point arithmetic module with a functor              *)
(* --------------------------------------------------------- *)

(* --------------------------------------------------------- *)
(*  The signature FRACTIONAL_BITS                             *)
(* --------------------------------------------------------- *)
module type FRACTIONAL_BITS = 
  sig
    val bits : int
  end

(* --------------------------------------------------------- *)
(*  The signature FIXED                                      *)
(* --------------------------------------------------------- *)
module type FIXED = sig
  type t
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool        (** physical equality *)
  val eqs : t -> t -> bool        (** structural equality *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

(* --------------------------------------------------------- *)
(*  The Signature MAKE                                       *)
(* --------------------------------------------------------- *)
module type MAKE = 
  functor (FB : FRACTIONAL_BITS) -> FIXED

(* --------------------------------------------------------- *)
(*  The Functor MAKE                                        *)
(* --------------------------------------------------------- *)

module Make : MAKE =
  functor (FB : FRACTIONAL_BITS) ->
  struct
    (* Use a record to store the internal int value *)
    type t = { value : int }

    let roundf x =
      if x >= 0. then floor (x +. 0.5)
      else ceil  (x -. 0.5)

    let scale = 1 lsl FB.bits
    let scale_f = float_of_int scale

    (* Conversion functions *)
    let of_float fl = { value = int_of_float (roundf (fl *. scale_f)) }
    let of_int i = { value = i lsl FB.bits }
    let to_float x = (float_of_int x.value) /. scale_f
    let to_int x = x.value asr FB.bits  (* preserves sign *)

    let to_string t = string_of_float (to_float t)

    (* Constants *)
    let zero = { value = 0 }
    let one  = of_int 1

    (* Successor / Predecessor *)
    let succ x = { value = x.value + 1 }
    let pred x = { value = x.value - 1 }

    (* Min / Max *)
    let min x y = if x.value <= y.value then x else y
    let max x y = if x.value >= y.value then x else y

    (* Comparisons *)
    let gth x y = x.value > y.value
    let lth x y = x.value < y.value
    let gte x y = x.value >= y.value
    let lte x y = x.value <= y.value

    (* Equality *)
    let eqp x y = x == y       (* physical equality: different record instances → false *)
    let eqs x y = x = y        (* structural equality: same value → true *)

    (* Arithmetic *)
    let add x y = { value = x.value + y.value }
    let sub x y = { value = x.value - y.value }
    let mul x y = of_float ((to_float x) *. (to_float y))
    let div x y =
      if y.value = 0 then raise Division_by_zero
      else of_float ((to_float x) /. (to_float y))

    (* Iteration over a range *)
    let foreach x y fn =
      if x.value <= y.value then
        let rec up i =
          if i.value > y.value then ()
          else (fn i; up (succ i))
        in up x
      else
        let rec down i =
          if i.value < y.value then ()
          else (fn i; down (pred i))
        in down x
  end

(* --------------------------------------------------------- *)
(*  Testing the Fixed-point Modules                          *)
(* --------------------------------------------------------- *)
module Fixed4 = Make(struct let bits = 4 end)
module Fixed8 = Make(struct let bits = 8 end)


let print s = print_endline s

let test_fixed (module F : FIXED) name =
  print ("--- Testing " ^ name ^ " ---");

  (* ---------- Conversion Tests ---------- *)
  let a = F.of_int 5 in
  print ("of_int 5 -> to_int: " ^ string_of_int (F.to_int a));
  (* Expected: 5 *)

  let b = F.of_float 3.5 in
  print ("of_float 3.5 -> to_float: " ^ string_of_float (F.to_float b));
  (* Expected approx: 3.5 *)

  (* Negative conversion *)
  let c = F.of_float (-2.25) in
  print ("of_float (-2.25) -> to_float: " ^ string_of_float (F.to_float c));
  (* Expected approx: -2.25 *)

  (* ---------- Basic Arithmetic ---------- *)
  let s = F.add a b in
  print ("5 + 3.5 = " ^ F.to_string s);
  (* Expected approx: 8.5 *)

  let d = F.sub a b in
  print ("5 - 3.5 = " ^ F.to_string d);
  (* Expected approx: 1.5 *)

  let m = F.mul a b in
  print ("5 * 3.5 = " ^ F.to_string m);
  (* Expected approx: 17.5 *)

  let dv = F.div a b in
  print ("5 / 3.5 = " ^ F.to_string dv);
  (* Expected approx: 1.428... *)

  (* division by zero → returns 0 by your implementation *)
  (* add try except *)
  let dvz =
    try F.div a F.zero
    with Division_by_zero -> print ("Division by zero encountered."); F.zero
  in
  print ("5 / 0 = " ^ F.to_string dvz);
  (* Expected: 0 *)
  (* let dvz = F.div a F.zero in
  print ("5 / 0 = " ^ F.to_string dvz); *)
  (* Expected: 0 *)

  (* ---------- Comparison ---------- *)
  print ("gth 5 3.5 -> " ^ string_of_bool (F.gth a b));
  (* Expected: true *)

  print ("lth 5 3.5 -> " ^ string_of_bool (F.lth a b));
  (* Expected: false *)

  print ("gte 5 5 -> " ^ string_of_bool (F.gte a a));
  (* Expected: true *)

  print ("lte 3.5 5 -> " ^ string_of_bool (F.lte b a));
  (* Expected: true *)

  (* ---------- Equality ---------- *)
  let a2 = F.of_int 5 in

  print ("eqp a a -> " ^ string_of_bool (F.eqp a a));
  (* Expected: true *)

  print ("eqp a a2 -> " ^ string_of_bool (F.eqp a a2));
  (* Expected: false (different int values in memory) *)

  print ("eqs a a2 -> " ^ string_of_bool (F.eqs a a2));
  (* Expected: true (same value) *)

  (* ---------- Succ / Pred ---------- *)
  print ("succ 5 -> " ^ F.to_string (F.succ a));
  (* Expected approx: 5 + 1/scale *)

  print ("pred 5 -> " ^ F.to_string (F.pred a));
  (* Expected approx: 5 - 1/scale *)

  (* ---------- min/max ---------- *)
  print ("min(5, 3.5) = " ^ F.to_string (F.min a b));
  (* Expected: 3.5 *)

  print ("max(5, 3.5) = " ^ F.to_string (F.max a b));
  (* Expected: 5 *)

  (* ---------- foreach ---------- *)
  print "foreach from 0 to 1:";
  F.foreach F.zero F.one (fun t ->
    print ("  value: " ^ F.to_string t)
  );
  (*
    Expected:

    foreach from 0 to 1:
      value: 0.0
      value: 0.0625   (for Fixed4 bits = 4)
        or
      value: 0.00390625 (for Fixed8 bits = 8)
      ...
      value: 1.0
  *)

  print ("--- End of " ^ name ^ " ---\n")

let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);

  Fixed4.foreach Fixed4.zero Fixed4.one
    (fun f -> print_endline (Fixed4.to_string f));
  

  print_endline "\n\nTesting Fixed-point Modules:\n";
  test_fixed (module Fixed4) "Fixed4 (bits=4)";
  test_fixed (module Fixed8) "Fixed8 (bits=8)";

()
(* Signature (Interface) of a pair module *)
module type PAIR = sig 
  val pair : (int * int)  
end

(* Signature (Interface) of a value module *)
module type VAL = sig
  val x : int 
end

(* fst and snd are built-in functions from the standard library*)
module MakeFst (P : PAIR) : VAL = struct
  let x = fst P.pair
end

module MakeSnd (P : PAIR) : VAL = struct
  let x = snd P.pair
end

module Pair : PAIR = struct let pair = ( 21, 42 ) end
module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)
let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
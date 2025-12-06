(* ---------------- VAL signature -------------------------- *)
module type VAL =
sig
  type t
  val add : t -> t -> t
  val mul : t -> t -> t
end


(* ---------------- EVALEXPR signature -------------------- *)
module type EVALEXPR = 
sig
  type t
  type expr = Value of t 
            | Add of expr * expr
            | Mul of expr * expr
  val eval : expr -> t
end


(* ---------------- MAKEEVALEXPR signature ---------------- *)
module type MAKEEVALEXPR =
  functor (V : VAL) -> EVALEXPR with type t = V.t


  (* ---------------- MakeEvalExpr functor ------------------ *)
module MakeEvalExpr : MAKEEVALEXPR =
  functor (V : VAL) -> struct
    type t = V.t
    type expr = Value of t 
              | Add of expr * expr
              | Mul of expr * expr

    let rec eval = function
      | Value x -> x
      | Add (x, y) -> V.add (eval x) (eval y)
      | Mul (x, y) -> V.mul (eval x) (eval y)
  end


  (* ---------------- Concrete VAL modules ------------------ *)
module IntVal : (VAL with type t = int) = 
struct
  type t = int
  let add = ( + )
  let mul = ( * )
end

module FloatVal : (VAL with type t = float) = 
struct
  type t = float
  let add = ( +. )
  let mul = ( *. )
end

module StringVal : (VAL with type t = string) = 
struct
  type t = string
  let add s1 s2 = if String.length s1 > String.length s2 then s1 else s2
  let mul = ( ^ )
end


(* ---------------- EvalExpr modules ---------------------- *)
module IntEvalExpr    : (EVALEXPR with type t := IntVal.t) = MakeEvalExpr(IntVal)
module FloatEvalExpr  : (EVALEXPR with type t := FloatVal.t) = MakeEvalExpr(FloatVal)
module StringEvalExpr : (EVALEXPR with type t := StringVal.t) = MakeEvalExpr(StringVal)

(* ---------------- Test expressions ---------------------- *)
let ie = IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2)
let fe = FloatEvalExpr.Add (FloatEvalExpr.Value 41.5, FloatEvalExpr.Value 0.92)
let se =
  StringEvalExpr.Mul (
    StringEvalExpr.Value "very ",
    StringEvalExpr.Add (
      StringEvalExpr.Value "very long",
      StringEvalExpr.Value "short"
    )
  )

(* ---------------- Print results ------------------------- *)
let () =
  Printf.printf "Res = %d\n" (IntEvalExpr.eval ie);
  Printf.printf "Res = %f\n" (FloatEvalExpr.eval fe);
  Printf.printf "Res = %s\n" (StringEvalExpr.eval se)

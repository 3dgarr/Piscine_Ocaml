open Monoid

module Calc =
functor (M : MONOID) ->
struct
  let add = M.add
  let sub = M.sub
  let mul = M.mul
  let div = M.div

  let power x n =
    let rec aux acc n =
      if n <= 0 then acc
      else aux (M.mul acc x) (n - 1)
    in
    aux M.zero2 n

  let fact x =
    let rec aux acc n =
      if n <= M.zero1 then acc
      else aux (M.mul acc n) (M.sub n M.zero2)
    in
    aux M.zero2 x
end

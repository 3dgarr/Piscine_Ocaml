module Watchtower :
sig
  type hour = int
  val zero : hour
  val add : hour -> hour -> hour
  val sub : hour -> hour -> hour
end
=
struct
  type hour = int

  let modulo = 12

  let normalize h =
    ((h mod modulo) + modulo) mod modulo

  let zero = 0

  let add h1 h2 =
    normalize (h1 + h2)

  let sub h1 h2 =
    normalize (h1 - h2)
end

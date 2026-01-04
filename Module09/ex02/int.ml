module INT =
struct
  type element = int

  let zero1 = 0     (* for add / sub *)
  let zero2 = 1     (* for mul / div *)

  let add a b = a + b
  let sub a b = a - b
  let mul a b = a * b
  let div a b = a / b
end

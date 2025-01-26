(* Write a function ft_test_sign of type int -> unit that displays "positive" or
"negative", each followed by a new line, depending on the sign of the parameter. The
value 0 is always considered positive. *)

let ft_test_sign n =
  if n >= 0 then
    print_endline "positive"
  else
    print_endline "negative"

let () =  ft_test_sign 42
let () =  ft_test_sign (-42)
let () =  ft_test_sign 0

(* how to compile this code *)
(*Simple Recursive implementation *)
let rec fibonacci_simple (n: int) : int =
    if n = 0 then 0
    else if n = 1 then 1
    else fibonacci_simple (n - 1) + fibonacci_simple (n - 2)


(* Tail recursive implementation *)
let fibonacci (n: int) : int =
  let rec go (n: int) (a: int) (b: int) : int =
    if n = 0 then a
    else if n = 1 then b
    else go (n - 1) b (a + b)
  in
  go n 0 1

let () =
  print_endline (string_of_int (fibonacci 0)) ;
  print_endline (string_of_int (fibonacci 1)) ;
  print_endline (string_of_int (fibonacci 2)) ;
  print_endline (string_of_int (fibonacci 3)) ;
  print_endline (string_of_int (fibonacci 4)) ;
  print_endline (string_of_int (fibonacci 5)) ;
  print_endline (string_of_int (fibonacci 6)) ;
  print_endline (string_of_int (fibonacci 7)) ;
  print_endline (string_of_int (fibonacci 8)) ;
  print_endline (string_of_int (fibonacci 9)) ;
  print_endline (string_of_int (fibonacci 10)) ;
  print_endline (string_of_int (fibonacci 50)) ;

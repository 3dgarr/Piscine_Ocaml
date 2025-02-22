
let rec ackermann : int -> int -> int = fun m n ->
  if n = -1 || m = -1 then
    -1
  else if m = 0 then
    n + 1
  else if n = 0 && m > 0 then
    ackermann (m - 1) 1
  else
    ackermann (m - 1) (ackermann m (n - 1))

    
let () =
    print_endline (string_of_int (ackermann (-1) 7)) ;
    print_endline (string_of_int (ackermann 0 0)) ;
    print_endline (string_of_int (ackermann 2 3)) ;
    print_endline (string_of_int (ackermann 4 1)) ;
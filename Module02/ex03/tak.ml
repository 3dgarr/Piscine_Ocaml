
let rec tak :int -> int -> int -> int = fun x y z ->
    if y < x then
        tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
    else
        z
        
    
let () =
    print_endline (string_of_int (tak   1 2 3)) ;
    print_endline (string_of_int (tak  5 23 7)) ;
    print_endline (string_of_int (tak  9 1 0)) ;
    print_endline (string_of_int (tak  1 1 1)) ;
    print_endline (string_of_int (tak  0 42 0)) ;
    print_endline (string_of_int (tak 23498 98734 98776)) ;

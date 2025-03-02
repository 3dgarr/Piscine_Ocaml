let rec converges (f:'a -> 'a) (x: 'a) (n: int): bool =
  if n < 0 then false
  else if fixed_point x = f x then true 
  else converges f (f x) (n - 1)

  and  fixed_point (arg: 'a): 'a = arg
  
let () =
  print_endline (string_of_bool (converges (( * ) 2) 2 5)) ;
  print_endline (string_of_bool (converges (fun x -> x / 2) 2 3)) ;
  print_endline (string_of_bool (converges (fun x -> x / 2) 2 2)) ;

    
(* 
print_endline ("----------------") ;
print_endline (string_of_int x) ;
print_endline (string_of_int (f x)) ;
print_endline ("----------------") ; 
*)

let rec iter (f:int -> int) (x: int) (n: int): int =
  if n < 0 then -1
  else if n  = 0 then x
  else iter f (f x) (n - 1)


let () =
  (* print_endline (string_of_int (iter (fun x -> x * 2) 2 4)) ; *)
  (* print_endline (string_of_int (iter (fun x -> x * x) 3 2)) ; *)
  (* print_endline (string_of_int (iter (fun x -> x + 5) 10 5  )) ; *)
  print_endline (string_of_int (iter (( * ) 2) 2 5 )) ;
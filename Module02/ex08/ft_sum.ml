let ft_sum (f:int -> float) (start:int) (finish:int) : float = 
  if finish < start then nan
  else 
    let rec loop (f:int -> float) (start:int) (finish:int) (acc:float) : float =
      if start > finish then acc
      else loop f (start + 1) finish (acc +. f start)
    in
      loop f start finish 0.0

let () = 
  print_endline (string_of_float (ft_sum (fun x -> float_of_int (x * x)) 1 10)) ;
  print_endline (string_of_float (ft_sum (fun x -> float_of_int (x * x)) 10 1)) ;
  print_endline (string_of_float (ft_sum (fun x -> float_of_int (x * x)) 1 1)) ;
  print_endline (string_of_float (ft_sum (fun x -> float_of_int (x * x)) 1 0)) ;
  print_endline (string_of_float (ft_sum (fun x -> float_of_int (x * x)) 0 1)) ;
  print_endline (string_of_float (ft_sum (fun x -> float_of_int (x * x)) 0 0)) ;
  print_endline (string_of_float (ft_sum (fun x -> float_of_int (x * 2)) 1 5)) ;



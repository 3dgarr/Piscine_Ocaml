(* 

let repeat_x n =
  if n < 0 then
    "Error"
  else
    let rec concat_x n output = 
      if n = 0 then
        output
      else
        concat_x (n - 1) (output ^ "x")
    in 
      concat_x n "" ;

  ;; *)

let repeat_x n =
  if n < 0 then
    "Error"
  else
    let rec concat_x n output = 
      if n = 0 then
        output
      else
        concat_x (n - 1) (output ^ "x")
    in 
      concat_x n "" ;

  ;;

(* Test *)
print_endline (repeat_x 0) ;
print_endline (repeat_x 1) ;
print_endline (repeat_x 5) ;
print_endline (repeat_x (-1)) ;
print_endline (repeat_x 10) ;
print_endline (repeat_x 100) ;
print_endline (repeat_x 1000) ;
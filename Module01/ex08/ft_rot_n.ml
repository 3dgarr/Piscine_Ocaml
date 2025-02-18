let is_uppercase c = c >= 'A' && c <= 'Z' 
let is_lowercase c = c >= 'a' && c <= 'z' 
let is_alpha c = is_lowercase c || is_uppercase c  

let get_base_of_char symbol = 
  if is_uppercase symbol then
    int_of_char 'A'
  else
    int_of_char 'a'

let ft_rot_n n str =
  let size_of_alphabet = 26 in
  let shift_char symbol = 
    if is_alpha symbol then
      char_of_int ((int_of_char symbol - get_base_of_char symbol + n) mod size_of_alphabet + get_base_of_char symbol)
    else
      symbol
  in 
  String.map shift_char str

let () = 
  print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz"); 
  (* Output should be  -->	"bcdefghijklmnopqrstuvwxyza" *)
  print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz"); 
  (* Output should be  -->	"nopqrstuvwxyzabcdefghijklm" *)
  print_endline (ft_rot_n 42 "0123456789"); 
  (* Output should be  -->	"0123456789" *)
  print_endline (ft_rot_n 2 "OI2EAS67B9"); 
  (* Output should be  -->	"QK2GCU67D9" *)
  print_endline (ft_rot_n 0 "Damned !"); 
  (* Output should be  -->	"Damned !" *)
  print_endline (ft_rot_n 42 ""); 
  (* Output should be  -->	"" *)
  print_endline (ft_rot_n 1 "NBzlk qnbjr !") 
  (* Output should be  -->	"OCaml rocks !" *)
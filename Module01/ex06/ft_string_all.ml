let is_digit c = c >= '0' && c <= '9'
let is_uppercase c = c >= 'A' && c <= 'Z' 
let is_lowercase c = c >= 'a' && c <= 'z' 
let is_alpha c = is_lowercase c || is_uppercase c  

let ft_string_all func str = 
  let rec iter func str index =
    if index < 0 then true
    else
        func (String.get str index) && (iter func str (index - 1));
    in 
    iter func str (String.length str - 1)



let () =
  print_endline (string_of_bool (ft_string_all is_digit "0123456789")) ;
  print_endline (string_of_bool (ft_string_all is_digit "O12EAS67B9")) ;
  print_endline (string_of_bool (ft_string_all is_digit "BAREEEEEEEV")) ;
  print_endline (string_of_bool (ft_string_all is_alpha "BAREEEEEEEV")) ;
  print_endline (string_of_bool (ft_string_all is_uppercase "BAREEEEEEEV")) ;
  print_endline (string_of_bool (ft_string_all is_lowercase "BAREEEEEEEV")) ;
  print_endline (string_of_bool (ft_string_all is_digit "0123456789a")) ;
  print_endline (string_of_bool (ft_string_all is_digit "a0123456789")) ;
  print_endline (string_of_bool (ft_string_all is_digit "8888888888888888"));
let ft_is_palindrome str = 
  let rec check_palindrome str start_index end_index = 
    if start_index >= end_index then 
      true
    else if (String.get str start_index) <> (String.get str end_index) then 
      false
    else
      check_palindrome str (start_index + 1) (end_index - 1)
  in 
    check_palindrome str 0 (String.length str - 1)


let () = 
  print_endline (string_of_bool (ft_is_palindrome "0123456789")) ;   
  print_endline (string_of_bool (ft_is_palindrome "madam")) ;   
  print_endline (string_of_bool (ft_is_palindrome "apapa")) ;   
  print_endline (string_of_bool (ft_is_palindrome "radar")) ;   
  print_endline (string_of_bool (ft_is_palindrome "car")) ;   
  print_endline (string_of_bool (ft_is_palindrome "")) ;   
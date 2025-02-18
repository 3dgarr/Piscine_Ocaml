let is_even number = (number mod 2) = 0

let rec ft_power base exp =
  if exp = 0 then 
    1
  else if is_even exp then
    ft_power (base * base) (exp / 2)
  else 
    base * ft_power (base * base) ((exp - 1) / 2)

let () =
  let result = ft_power 3 1 in print_int result; print_char '\n';
  let result = ft_power 2 2 in print_int result; print_char '\n';
  let result = ft_power 2 4 in print_int result; print_char '\n';
  let result = ft_power 3 0 in print_int result; print_char '\n';
  let result = ft_power 0 5 in print_int result; print_char '\n';
  let result = ft_power 2 15 in print_int result; print_char '\n'
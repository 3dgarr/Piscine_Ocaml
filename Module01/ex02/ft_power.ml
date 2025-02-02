let is_even number = (number mod 2) = 0;;
let is_odd number = (number mod 2) <> 0;;

let rec ft_power base exp =
	if exp = 0 then 
		1
	else if is_even exp then
		ft_power (base * base) (exp / 2)
	else 
		base * ft_power (base *  base) ((exp - 1) / 2)
;; 


print_int (ft_power 3  1);
print_char '\n';
print_int (ft_power 2  2);
print_char '\n';
print_int (ft_power 2  4);
print_char '\n';
print_int (ft_power 3  0);
print_char '\n';
print_int (ft_power 0  5);
print_char '\n';
print_int (ft_power 2 15);
print_char '\n';


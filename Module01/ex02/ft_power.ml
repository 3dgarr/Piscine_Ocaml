(* 
In: a real number x; an integer n
Out: xn

exp_by_squaring(x, n)
  if n < 0 then
	 return exp_by_squaring(1 / x, -n);
  else if n = 0 then 
	 return 1;
  else if n is even then 
	 return exp_by_squaring(x * x, n / 2);
  else if n is odd then 
	 return x * exp_by_squaring(x * x, (n - 1) / 2);
end function 
 *)

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


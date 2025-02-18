let print_number decimal unit = 
  if decimal < 10 then print_int 0;
  print_int decimal;
  print_char ' ';
  if unit < 10 then print_int 0;
  print_int unit;
  if decimal == 98 && unit == 99 then
    print_char '\n'
  else
    begin
      print_char ',';
      print_char ' '
    end


let rec get_unit decimal unit = 
  if unit <= 99 then
    begin 
      print_number decimal unit ;
      get_unit decimal (unit + 1)
    end


let rec get_decimal decimal unit  = 
  if decimal <= 98 then
    begin
      get_unit  decimal unit ;
      get_decimal (decimal + 1) (unit + 1)
    end



let ft_print_comb2 () = 
  let decimal = 0 in
  let unit = decimal + 1 in
  get_decimal decimal unit


let () = ft_print_comb2 ()
let ft_print_alphabet = 
  let rec print_symbol symbol = 
    if int_of_char symbol > int_of_char 'z' then
      ()
    else
      begin
        print_char symbol;
        print_symbol (char_of_int (int_of_char symbol + 1)) ;
      end
    in      
      begin
        print_symbol 'a';
        print_char '\n';
      end


let () = ft_print_alphabet
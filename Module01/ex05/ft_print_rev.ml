let ft_print_rev str = 
  let rec print_symbol str index = 
    if index <= 0 then 
      print_char '\n'
    else
      begin
        print_char (String.get str (index - 1));
        print_symbol str (index - 1)
      end
  in
    print_symbol str (String.length str);
;;

let () = ft_print_rev "barev"
let () = ft_print_rev "Hello World!"
let () = ft_print_rev ""
let () = ft_print_rev "1234567890"
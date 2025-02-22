
let repeat_string : ?str:string -> int -> string = fun ?str:(str="x") n ->
  if n < 0 then
    "Error"
  else
    let rec concat_string n output = 
      if n  = 0 then
        output
      else
        concat_string (n - 1) (output ^ str)
    in
      concat_string n ""


  let () =
    print_endline (repeat_string (-1));
    print_endline (repeat_string 0);
    print_endline (repeat_string ~str:"Toto" 1);
    print_endline (repeat_string 2);
    print_endline (repeat_string ~str:"a" 5);
    print_endline (repeat_string ~str:"what" 3);
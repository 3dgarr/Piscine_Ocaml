
let () = 
  let p1 = new People.people "Alice" in
  let p2 = new People.people "Bob" in
  print_endline (p1#to_string);
  print_endline (p1#talk);

  print_endline (p2#to_string);
  print_endline (p2#talk);
  
  print_endline (p1#die);
  print_endline (p2#die);
()
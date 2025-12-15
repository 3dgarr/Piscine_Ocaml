
let () = 
  let hydrogen = new Atom.hydrogen in
  let carbon = new Atom.carbon in
  let oxygen = new Atom.oxygen in
  let chlorine = new Atom.chlorine in
  let oganesson = new Atom.oganesson in
  let molybdenum = new Atom.molybdenum in
  print_endline (hydrogen#to_string);
  print_endline (carbon#to_string);
  print_endline (oxygen#to_string);
  print_endline (chlorine#to_string);
  print_endline (oganesson#to_string);
  print_endline (molybdenum#to_string);
  
  print_endline (if hydrogen#equals (new Atom.hydrogen) then "Hydrogen equals Hydrogen" else "Hydrogen does not equal Hydrogen");
  print_endline (if hydrogen#equals carbon then "Hydrogen equals Carbon" else "Hydrogen does not equal Carbon");
  ()
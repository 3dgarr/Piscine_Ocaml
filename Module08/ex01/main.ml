open Molecules

let () =
  let w = new water in
  let co2 = new carbon_dioxide in
  let m = new methane in
  let a = new ammonia in
  let e = new ethanol in
  let tnt = new trinitrotoluene in

  List.iter
    (fun m -> print_endline m#to_string)
    [ w; co2; m; a; e; tnt ];
    
  print_endline ("\nTesting equality:\n");
  
  let w2 = new water in
  let co2_2 = new carbon_dioxide in
  let m2 = new methane in
  let a2 = new ammonia in
  let e2 = new ethanol in

  let test_equality m1 m2 =
    Printf.printf "%s and %s are equal: %b\n" m1#name m2#name (m1#equals m2)
  in
  test_equality w w2;
  test_equality co2 co2_2;
  test_equality m m2;
  test_equality a a2;
  test_equality e e2;
  test_equality w co2;
  test_equality m a;
  test_equality e w


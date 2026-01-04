open Int
open Float
open Calc

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let () =
  (* POWER TESTS *)
  assert (Calc_int.power 3 3 = 27);
  assert (Calc_int.power 5 0 = 1);
  assert (Calc_float.power 2.0 3 = 8.0);
  assert (Calc_float.power 10.0 1 = 10.0);
  print_endline "Power tests passed";

  (* ADD / SUB TESTS *)
  assert (Calc_int.add 20 1 = 21);
  assert (Calc_int.sub 20 5 = 15);
  assert (Calc_float.add 20.5 1.5 = 22.0);
  assert (Calc_float.sub 10.5 0.5 = 10.0);
  print_endline "Add / Sub tests passed";

  (* MUL / DIV TESTS *)
  assert (Calc_int.mul 4 5 = 20);
  assert (Calc_int.div 20 4 = 5);
  assert (Calc_float.mul 2.5 2.0 = 5.0);
  assert (Calc_float.div 5.0 2.0 = 2.5);
  print_endline "Mul / Div tests passed";

  (* COMPOSED OPERATIONS *)
  assert (Calc_int.mul (Calc_int.add 20 1) 2 = 42);
  assert (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0 = 42.0);
  print_endline "Composed operations tests passed";

  (* FACTORIAL TESTS *)
  assert (Calc_int.fact 5 = 120);
  assert (Calc_int.fact 0 = 1);
  assert (Calc_float.fact 4.0 = 24.0);
  print_endline "Factorial tests passed";

  (* PRINT TESTS (requested by subject style) *)
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));

  
  print_endline "All Calc tests passed"

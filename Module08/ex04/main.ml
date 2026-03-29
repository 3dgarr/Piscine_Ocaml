open Alkanes
open Alkane_combustion

let print_sep () = print_endline (String.make 50 '-')

let test_combustion name alkanes =
  print_endline ("=== " ^ name ^ " ===");
  let r = new alkane_combustion alkanes in
  print_endline ("Unbalanced reaction : " ^ r#to_string);
  print_endline ("is_balanced          : " ^ string_of_bool r#is_balanced);
  (try
     let _ = r#get_start in
     print_endline "get_start on unbalanced: BUG — should have raised"
   with Unbalanced ->
     print_endline "get_start on unbalanced: correctly raised Unbalanced");
  (try
     let _ = r#get_result in
     print_endline "get_result on unbalanced: BUG — should have raised"
   with Unbalanced ->
     print_endline "get_result on unbalanced: correctly raised Unbalanced");
  let b = r#balance in
  print_endline ("Balanced reaction    : " ^ b#to_string);
  print_endline ("is_balanced          : " ^ string_of_bool b#is_balanced);
  print_endline "Start molecules:";
  List.iter
    (fun (m, c) -> print_endline ("  " ^ string_of_int c ^ " x " ^ m#formula))
    b#get_start;
  print_endline "Result molecules:";
  List.iter
    (fun (m, c) -> print_endline ("  " ^ string_of_int c ^ " x " ^ m#formula))
    b#get_result;
  print_sep ()

let () =
  print_endline "========== ALKANE COMBUSTION TESTS ==========\n";

  (* CH4 + 2O2 -> CO2 + 2H2O *)
  test_combustion "Methane (C1)" [ new methane ];

  (* 2C2H6 + 7O2 -> 4CO2 + 6H2O *)
  test_combustion "Ethane (C2)" [ new ethane ];

  (* C3H8 + 5O2 -> 3CO2 + 4H2O *)
  test_combustion "Propane (C3)" [ new propane ];

  (* 2C4H10 + 13O2 -> 8CO2 + 10H2O *)
  test_combustion "Butane (C4)" [ new butane ];

  (* C8H18 + ... *)
  test_combustion "Octane (C8)" [ new octane ];

  (* Duplicate alkanes should be merged: two methanes *)
  test_combustion "2x Methane (duplicates)" [ new methane; new methane ];

  (* Mixed alkanes *)
  test_combustion "Methane + Propane" [ new methane; new propane ];

  (* Large alkane *)
  test_combustion "Dodecane (C12)" [ new dodecane ];

  print_endline "All tests completed."
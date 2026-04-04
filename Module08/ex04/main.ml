open Alkane_combustion

let () =
  let test_alkane (name : string) (alk : #Alkane.alkane) =
    print_endline ("Test: " ^ name);
    
    (* 1. Unbalanced state *)
    let comb = new alkane_combustion [ (alk :> Alkane.alkane) ] in
    
    Printf.printf "  to_string (unbalanced): %s\n" comb#to_string;
    
    if not comb#is_balanced then
      print_endline "  ✓ is_balanced = false"
    else
      print_endline "  ✗ is_balanced should be false";

    (try
       let _ = comb#get_start in
       print_endline "  ✗ get_start should raise Unbalanced"
     with
     | Unbalanced -> print_endline "  ✓ get_start raises Unbalanced"
     | _ -> print_endline "  ✗ wrong exception from get_start");

    (try
       let _ = comb#get_result in
       print_endline "  ✗ get_result should raise Unbalanced"
     with
     | Unbalanced -> print_endline "  ✓ get_result raises Unbalanced"
     | _ -> print_endline "  ✗ wrong exception from get_result");

    (* 2. Balance it *)
    let balanced = comb#balance in

    Printf.printf "  to_string (balanced):   %s\n" balanced#to_string;

    if balanced#is_balanced then
      print_endline "  ✓ is_balanced = true"
    else
      print_endline "  ✗ is_balanced should be true";

    (try
       let _start = balanced#get_start in
       let _result = balanced#get_result in
       print_endline "  ✓ get_start and get_result succeeded on balanced object"
     with
     | Unbalanced -> print_endline "  ✗ balanced object raised Unbalanced"
     | e -> Printf.printf "  ✗ unexpected exception: %s\n" (Printexc.to_string e));

    print_endline ""
  in

  (* === Simple test cases === *)
  test_alkane "Methane" (new Alkanes.methane);
  test_alkane "Ethane"  (new Alkanes.ethane);
  test_alkane "Propane" (new Alkanes.propane);

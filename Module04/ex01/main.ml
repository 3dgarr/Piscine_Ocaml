let () =

  let open Value in

  print_endline "Testing toInt function:";
  let test_toInt value =
    Printf.printf "Value: %s, toInt: %d\n" (toString value) (toInt value)
  in
  List.iter test_toInt all; 

  print_endline "Testing toString function:";
  let test_toString value =
    Printf.printf "Value: %s, toString: %s\n" (toString value) (toString value)
  in
  List.iter test_toString all;

  print_endline "Testing toStringVerbose function:";
  let test_toStringVerbose value =
    Printf.printf "Value: %s, toStringVerbose: %s\n" (toString value) (toStringVerbose value)
  in
  List.iter test_toStringVerbose all;

  print_endline "Testing next function:";
  let test_next value =
    try
      let next_value = next value in
      Printf.printf "Value: %s, Next: %s\n" (toString value) (toString next_value)
    with Invalid_argument msg ->
      Printf.printf "Value: %s, Error: %s\n" (toString value) msg
  in
  List.iter test_next all;

  print_endline "Testing prev function:";
  let test_prev value =
    try
      let prev_value = previous value in
      Printf.printf "Value: %s, Previous: %s\n" (toString value) (toString prev_value)
    with Invalid_argument msg ->
      Printf.printf "Value: %s, Error: %s\n" (toString value) msg
  in
  List.iter test_prev all;


  print_endline "Testing completed.";
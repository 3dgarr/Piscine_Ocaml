(* testing the Value.ml file *)
(* 
content fo Value.ml
type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As] 

let toInt = function
  | T2 -> 1
  | T3 -> 2
  | T4 -> 3
  | T5 -> 4
  | T6 -> 5
  | T7 -> 6
  | T8 -> 7
  | T9 -> 8
  | T10 -> 9
  | Jack -> 10
  | Queen -> 11
  | King -> 12
  | As -> 13

let toString = function
  | T2 -> "2"
  | T3 -> "3"
  | T4 -> "4"
  | T5 -> "5"
  | T6 -> "6"
  | T7 -> "7"
  | T8 -> "8"
  | T9 -> "9"
  | T10 -> "10"
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"
  | As -> "As"


let toVerbose = function
  | T2 -> "2"
  | T3 -> "3"
  | T4 -> "4"
  | T5 -> "5"
  | T6 -> "6"
  | T7 -> "7"
  | T8 -> "8"
  | T9 -> "9"
  | T10 -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | As -> "A"

let next = function
  | T2 -> T3
  | T3 -> T4
  | T4 -> T5
  | T5 -> T6
  | T6 -> T7
  | T7 -> T8
  | T8 -> T9
  | T9 -> T10
  | T10 -> Jack
  | Jack -> Queen
  | Queen -> King
  | King -> As
  | As -> invalid_arg "No next value for As"

let prev = function
  | T2 -> invalid_arg "No previous value for T2"
  | T3 -> T2
  | T4 -> T3
  | T5 -> T4
  | T6 -> T5
  | T7 -> T6
  | T8 -> T7
  | T9 -> T8
  | T10 -> T9
  | Jack -> T10
  | Queen -> Jack
  | King -> Queen
  | As -> King
*)
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

  print_endline "Testing toVerbose function:";
  let test_toVerbose value =
    Printf.printf "Value: %s, toVerbose: %s\n" (toString value) (toVerbose value)
  in
  List.iter test_toVerbose all;

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
      let prev_value = prev value in
      Printf.printf "Value: %s, Previous: %s\n" (toString value) (toString prev_value)
    with Invalid_argument msg ->
      Printf.printf "Value: %s, Error: %s\n" (toString value) msg
  in
  List.iter test_prev all;


  print_endline "Testing completed.";
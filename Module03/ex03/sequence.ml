let get_nth_of_look_and_say_sequence (n: int) =
  let make_string_manual (count: int) (digit: char) =
    let count_str = string_of_int count in
    let digit_to_str =  function
      | '0' -> "0"
      | '1' -> "1"
      | '2' -> "2"
      | '3' -> "3"
      | '4' -> "4"
      | '5' -> "5"
      | '6' -> "6"
      | '7' -> "7"
      | '8' -> "8"
      | '9' -> "9"
      | _   -> failwith "Invalid digit"
    in
    let digit_str = digit_to_str digit in
    count_str ^ digit_str
  in 
  let rec look_and_say n =
    if n < 1 then failwith "n must be >= 1"
    else if n = 1 then "1"
    else
      let prev = look_and_say (n - 1) in
      let rec aux count acc =
        match acc with
        | [] -> []
        | h :: t ->  

      (*TODO:   *)
  in
  look_and_say n

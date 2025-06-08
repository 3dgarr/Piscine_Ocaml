let rec append lst1 lst2 =
	match lst1 with
	| [] -> lst2
	| head :: tail -> head :: append tail lst2
and 

	int_to_char_list n =
    if n < 10 then [char_of_int (n + 48)]
    else 
      append (int_to_char_list (n / 10)) [char_of_int ((n mod 10) + 48)]
 and
	char_list_to_string lst =
    let char_digit_to_string c =
      if c = '0' then "0"
      else if c = '1' then "1"
      else if c = '2' then "2"
      else if c = '3' then "3"
      else if c = '4' then "4"
      else if c = '5' then "5"
      else if c = '6' then "6"
      else if c = '7' then "7"
      else if c = '8' then "8"
      else if c = '9' then "9"
      else ""
    in
      match lst with
      | [] -> ""
      | h :: t -> char_digit_to_string h ^ char_list_to_string t
 
let sequence n =
	if n < 1 then ""
	else
		let rec new_list input output count =
		match input with
		| [] -> output
		| head :: next :: tail when head = next -> 
			new_list (next :: tail) output (count + 1)
		| head :: tail ->
			let count_chars = int_to_char_list (count + 1) in
			let updated = append output (append count_chars [head]) in
			new_list tail updated 0
		in
			let rec sequence_aux step current =
			if step = n then char_list_to_string current
			else
				let next = new_list current [] 0 in
				sequence_aux (step + 1) next
			in
				sequence_aux 1 ['1']

let () =
    let n = read_int() in
    let result = sequence n in
    print_endline result

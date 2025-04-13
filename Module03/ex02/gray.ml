let rec gray (n: int) =
  if n = 0 then [""]
  else
	let prev = gray (n - 1) in
	let first = add_prefix "0" prev in
	let second = add_prefix "1" (reverse prev) in
	append first second

and  add_prefix (prefix: string) (lst: string list) =
	match lst with
	| [] -> []
	| h :: t -> (prefix ^ h) :: (add_prefix prefix t)
  
and reverse (lst: string list) =
	let rec aux acc lst =
	  match lst with
	  | [] -> acc
	  | h :: t -> aux (h :: acc) t
	in
	aux [] lst
  
and append (l1: string list) (l2: string list) =
	match l1 with
	| [] -> l2
	| h :: t -> h :: (append t l2)
  
and print_list (lst: string list) =
	match lst with
	| [] -> print_newline ()
	| h :: t ->
	  print_endline h;
	  print_list t
  
let () =
	let n = read_int () in
	let result = gray n in
	print_list result
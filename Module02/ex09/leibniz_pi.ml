let leibniz_pi (delta: float): int =
	if delta <= 0.0 then -1
	else 
	  let rec loop (acc: float) (i: int): int =
		let term = 4.0 *. ((-1.0) ** float_of_int i) /. (2.0 *. float_of_int i +. 1.0) in
		let temp = acc +. term in
		let pi_value = 4.0 *. atan 1.0 in
		let abs_value (x: float): float = if x < 0.0 then -. x else x in
		if abs_value (temp -. pi_value) <= delta then 
			begin
				print_endline ("Pi Value for delta " ^ string_of_float delta ^     " = " ^ string_of_float temp);			
				i
			end
		else loop temp (i + 1)
	  in
	  loop 0.0 0
  
let () =
	  print_endline ("Iterations for leibniz_pi -1.  = " ^ string_of_int (leibniz_pi (-1.)));
	  print_endline "---------------------------------";
	  print_endline ("Iterations for leibniz_pi 1.  = " ^ string_of_int (leibniz_pi 1.));
	  print_endline "---------------------------------";
	  print_endline ("Iterations for leibniz_pi 2.  = " ^ string_of_int (leibniz_pi 2.));
	  print_endline "---------------------------------";
	  print_endline ("Iterations for leibniz_pi 1e-0  = " ^ string_of_int (leibniz_pi 1e-0));
	  print_endline "---------------------------------";
	  print_endline ("Iterations for leibniz_pi 1e-1  = " ^ string_of_int (leibniz_pi 1e-1));
	  print_endline "---------------------------------";
	  print_endline ("Iterations for leibniz_pi 1e-2  = " ^ string_of_int (leibniz_pi 1e-2));
	  print_endline "---------------------------------";
	  print_endline ("Iterations for leibniz_pi 1e-3  = " ^ string_of_int (leibniz_pi 1e-3));
	  print_endline "---------------------------------";
	  print_endline ("Iterations for leibniz_pi 1e-4  = " ^ string_of_int (leibniz_pi 1e-4));
	  print_endline "---------------------------------";
	  print_endline ("Iterations for leibniz_pi 1e-5  = " ^ string_of_int (leibniz_pi 1e-5));
	  print_endline ("Iterations for leibniz_pi 1e-6  = " ^ string_of_int (leibniz_pi 1e-6));
	  print_endline "---------------------------------";
	  print_endline ("Iterations for leibniz_pi 1e-7  = " ^ string_of_int (leibniz_pi 1e-7));
	  print_endline "---------------------------------";
	  print_endline ("Iterations for leibniz_pi 1e-8  = " ^ string_of_int (leibniz_pi 1e-8));	  

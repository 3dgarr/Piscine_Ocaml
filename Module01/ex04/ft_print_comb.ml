let print hundred decimal unit =
    print_int hundred;
    print_int decimal;
    print_int unit;
    if hundred = 7 && decimal = 8 && unit = 9 then 
		print_string "\n"
    else 
		print_string ", "


let rec get_units hundred decimal unit =
    if unit <= 9 then
        begin
            print hundred decimal unit;
            get_units hundred decimal (unit + 1)
        end

let rec get_decimals hundred decimal unit =
    if decimal <= 8 then
        begin
            get_units hundred decimal unit;
            get_decimals hundred (decimal + 1) (decimal + 2)
        end

let rec get_hundreds hundred decimal unit =
    if hundred <= 7 then
        begin
            get_decimals hundred decimal unit;
            get_hundreds (hundred + 1) (hundred + 2) (hundred + 3)
        end

let ft_print_comb () =
    let hundred = 0 in
	let decimal = hundred + 1 in
	let unit = decimal + 1 in
	get_hundreds hundred decimal unit


let () = ft_print_comb()

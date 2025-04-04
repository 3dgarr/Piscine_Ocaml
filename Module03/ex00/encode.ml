let encode lst =
    let rec aux (count: int) (acc: 'a list) = function
    | [] -> acc
    | [x] -> (count + 1, x) :: acc
    | a:: (b:: _ as t) -> 
        if a = b then aux (count + 1) acc t
        else aux 0 ((count +1, a) :: acc) t
    in
        let rec reverse lst acc = match lst with 
        | [] -> acc
        | x :: xs -> reverse xs (x :: acc)
in 
    reverse (aux 0 [] lst) []

  
let () =
    let encoded = encode ['a' ; 'a' ; 'a' ; 'a' ; 'a' ; 'b' ; 'b' ; 'b' ; 'b' ;'a' ; 'a' ; 'a' ; ] in
    List.iter (fun (count, elem) -> Printf.printf "%d%c" count elem) encoded;
    print_endline "" ;
    let encoded = encode [1 ; 1 ; 6 ; 1 ; 2 ; 2 ; 2 ; 2 ; 3 ; 3 ; 3 ; 3 ] in
    List.iter (fun (count, elem) -> Printf.printf "%d%d" count elem) encoded;
    print_endline ""; 
    let encoded = encode [1 ; 1 ; 6 ; 1 ; 2 ; 2 ; 2 ; 2 ; 3 ; 3 ; 3 ; 3 ] in
    List.iter (fun (count, elem) -> Printf.printf "%d%d" count elem) encoded;
    print_endline ""; 
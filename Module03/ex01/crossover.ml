let crossover (lst1:'a list) (lst2: 'a list) = 
    let rec reverse (lst: 'a list) (acc: 'a list) = match lst with 
      | [] -> acc
      | x :: xs -> reverse xs (x :: acc)
    and
    is_item_in_list (item: 'a) (lst: 'a list) = match lst with
      | [] -> false
      | x :: xs -> if x = item then true else is_item_in_list item xs
    and
    aux (lst1: 'a list) (lst2: 'a list) (acc: 'a list) = match lst1, lst2 with
      | [], [] -> acc
      | [], y::ys -> acc
      | x::xs, [] -> acc
      | x::xs, y::ys -> 
          if is_item_in_list x lst2 then aux xs ys (x :: acc)
          else if is_item_in_list y lst1 then aux xs ys (y :: acc)
          else aux xs ys acc
    in
    reverse (aux lst1 lst2 []) []

  
let () =
    let lst1 = [4; 5; 3; 5; 4] in
    let lst2 = [4; 5; 6; 7; 8] in
    let result = crossover lst1 lst2 in
    List.iter (Printf.printf "%d ") result;
    print_newline ();

    let lst1 = [1; 2; 3; 5; 4] in
    let lst2 = [] in
    let result = crossover lst2 lst1 in
    List.iter (Printf.printf "%d ") result;
    print_newline ();

    let lst1 = ['a'; 'b'; 'c'; 'd'] in
    let lst2 = ['c'; 'd'; 'e'; 'f'] in
    let result = crossover lst1 lst2 in
    List.iter (Printf.printf "%c ") result;
    print_newline ();

    let lst1 = ["barev"; "privet"; "hi"; "hola"] in
    let lst2 = ["bye"; "barev"; "boo"; "hi"] in
    let result = crossover lst1 lst2 in
    List.iter (Printf.printf "%s ") result;
    print_newline ();
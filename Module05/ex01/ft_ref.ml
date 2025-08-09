(* 
    Create a type ft_ref to reproduce the ref type, and implement the following functions:
      • return: ’a -> ’a ft_ref: creates a new reference.
      • get: ’a ft_ref -> ’a: Dereferences a reference.
      • set: ’a ft_ref -> ’a -> unit: Assigns a reference’s value.
      • bind: ’a ft_ref -> (’a -> ’b ft_ref) -> ’b ft_ref: 
      This one is a bit more complicated. It applies a 
      function to a reference to transform it. You can think of
      it as a more complicated set function
 *)

type 'a ft_ref = { mutable contents: 'a }

let return x = { contents = x }

let get r = r.contents

(* <- operator means assignment *)
let set r x = r.contents <- x

let bind r f = f (get r)

let () =
  let r = return 42 in
  Printf.printf "Initial value: %d\n" (get r);
  set r 100;
  Printf.printf "After set: %d\n" (get r);
  let new_r = bind r (fun x -> return (x + 1)) in
  Printf.printf "After bind: %d\n" (get new_r);
  print_endline "";
  
  let s = return "hello" in
  Printf.printf "Initial value: %s\n" (get s);
  set s "world";
  Printf.printf "After set: %s\n" (get s);
  let new_s = bind s (fun x -> return (x ^ "!")) in
  Printf.printf "After bind: %s\n" (get new_s);
  
  print_endline "";

  let f = return 3.14 in
  Printf.printf "Initial value: %f\n" (get f);
  set f 2.71;
  Printf.printf "After set: %f\n" (get f);
  let new_f = bind f (fun x -> return (x *. 2.0)) in
  Printf.printf "After bind: %f\n" (get new_f);


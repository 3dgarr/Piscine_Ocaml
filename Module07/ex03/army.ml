class ['a] army =
object (self)
  (* list of members (any type) *)
  val mutable members : 'a list = []

  (* getter *)
  method get_members = members

  (* add to the front *)
  method add_front (x : 'a) =
    members <- x :: members

  (* add to the back *)
  method add_back (x : 'a) =
    members <- members @ [x]

  (* delete from the front *)
  method delete_front =
    match members with
    | [] -> ()
    | _ :: tl -> members <- tl

  (* delete from the back *)
  method delete_back =
    match List.rev members with
    | [] -> ()
    | _ :: tl -> members <- List.rev tl

  (* to_string (requires objects to have to_string method) *)
  method print_army =
    print_endline "=== Army Members ===";
    List.iter
      (fun m -> print_endline (m#to_string))
      members
end

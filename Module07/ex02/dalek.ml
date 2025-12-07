class dalek =
  object (self)
    (* ------------ NAME (DalekXXX) ------------ *)
    val _name : string =
      let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" in
      let rand_char () = chars.[Random.int (String.length chars)] in
      let suffix = String.init 3 (fun _ -> rand_char ()) in
      "Dalek" ^ suffix

    (* ------------ ATTRIBUTES ------------ *)
    val mutable _hp : int = 100
    val mutable _shield : bool = true

    (* ------------ GETTERS ------------ *)
    method get_name = _name
    method get_hp = _hp
    method get_shield = _shield

    (* ------------ TO_STRING ------------ *)
    method to_string =
      "Dalek: " ^ _name ^
      " | HP: " ^ string_of_int _hp ^
      " | Shield: " ^ string_of_bool _shield

    (* ------------ TALK ------------ *)
    method talk =
      let expressions_list = [
        "Explain! Explain!";
        "Exterminate! Exterminate!";
        "I obey!";
        "You are the Doctor! You are the enemy of the Daleks!"
      ] in
      let rand_index = Random.int (List.length expressions_list) in
      print_endline (List.nth expressions_list rand_index);

    (* ------------ EXTERMINATE ------------ *)
    method exterminate (p : People.people) =
      _shield <- false;  (* disable shield to attack *)
      print_endline (self#get_name ^ " is exterminating " ^ p#to_string);
      print_endline (p#die);

    (* ------------ DIE ------------ *)
    method die =
      print_endline "Emergency Temporal Shift!"
  end

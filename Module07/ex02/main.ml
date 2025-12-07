let () =
  Random.self_init ();

  let human = new People.people "Poghos Petr" in

  (* Create a Dalek *)
  let dalek = new Dalek.dalek in

  print_endline "---- INITIAL STATE ----";
  print_endline (human#to_string);
  print_endline (dalek#to_string);

  print_endline "\n---- DALEK TALKS ----";
  dalek#talk;

  print_endline "\n---- EXTERMINATION ----";
  dalek#exterminate human;

  print_endline "\n---- DALEK STATE AFTER ATTACK ----";
  print_endline (dalek#to_string);

  print_endline "\n---- DALEK DIES ----";
  dalek#die;

  ()

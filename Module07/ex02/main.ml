let () =
  Random.self_init ();

  print_endline "\n========= BATTLE START =========\n";

  (* Create characters *)
  let human = new People.people "Rory Williams" in
  let doctor = new Doctor.doctor "The Doctor" 907 human in
  let dalek = new Dalek.dalek in

  print_endline "\n--- Initial States ---";
  print_endline (human#to_string);
  print_endline (doctor#to_string);
  print_endline (dalek#to_string);
  print_endline "";

  (* Dalek talks *)
  print_endline "--- Dalek Talks ---";
  dalek#talk;
  print_endline "";

  (* Dalek kills the human *)
  print_endline "--- Dalek Exterminates the Human ---";
  dalek#exterminate human;
  print_endline "";

  (* Doctor tries to fight the Dalek *)
  print_endline "--- Doctor Attacks Dalek ---";
  doctor#attack dalek;
  print_endline ("Dalek state: " ^ dalek#to_string);
  print_endline "";

  (* Dalek retaliates *)
  (* print_endline "--- Dalek Exterminates the Doctor ---";
  dalek#exterminate_doc doctor;
  print_endline ("Doctor state:\n" ^ doctor#to_string);
  print_endline ""; *)

  (* Doctor regenerates if needed *)
  print_endline "--- Doctor Regeneration Check ---";
  print_endline (doctor#regenerate_if_needed);
  print_endline ("Doctor after regen (if any):\n" ^ doctor#to_string);
  print_endline "";

  (* Dalek dies *)
  print_endline "--- Dalek is hit critically ---";
  dalek#die;
  print_endline "";

  print_endline "========= BATTLE ENDED =========\n"

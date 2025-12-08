let () =
  Random.self_init ();

  (* Create people *)
  let p1 = new People.people "Alice" in
  let p2 = new People.people "Bob" in

  (* Create a doctor *)
  let doc = new Doctor.doctor "Who" 900 p1 in

  (* Create daleks *)
  let d1 = new Dalek.dalek in
  let d2 = new Dalek.dalek in

  (* Create armies *)
  let people_army = new Army.army in
  let doctor_army = new Army.army in
  let dalek_army = new Army.army in

  (* Add members *)
  people_army#add_front p1;
  people_army#add_back p2;

  doctor_army#add_front doc;

  dalek_army#add_front d1;
  dalek_army#add_back d2;

  (* Print armies *)
  print_endline "\n--- PEOPLE ARMY ---";
  people_army#print_army;

  print_endline "\n--- DOCTOR ARMY ---";
  doctor_army#print_army;

  print_endline "\n--- DALEK ARMY ---";
  dalek_army#print_army;

  (* Delete and show updated armies *)
  print_endline "\nDeleting front of Dalek army...";
  dalek_army#delete_front;

  print_endline "--- DALEK ARMY NOW ---";
  dalek_army#print_army;

  print_endline "\nDeleting back of People army...";
  people_army#delete_back;

  print_endline "--- PEOPLE ARMY NOW ---";
  people_army#print_army;
  ()
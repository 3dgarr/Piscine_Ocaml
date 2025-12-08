let () =
  Random.self_init ();

  (* Create some humans *)
  let p1 = new People.people "Alice" in
  let p2 = new People.people "Bob" in

  (* A sidekick for the Doctor *)
  let sid = new People.people "Clara" in

  (* Create Doctors *)
  let doc1 = new Doctor.doctor "The Doctor" 900 sid in
  let doc2 = new Doctor.doctor "TimeLord" 1200 p1 in

  (* Create Daleks *)
  let d1 = new Dalek.dalek in
  let d2 = new Dalek.dalek in
  let d3 = new Dalek.dalek in

  (* Create Galifrey *)
  let war = new Galifrey.galifrey in

  (* Add members *)
  war#add_dalek d1;
  war#add_dalek d2;
  war#add_dalek d3;

  war#add_doctor doc1;
  war#add_doctor doc2;

  war#add_people p1;
  war#add_people p2;

  (* Start the war *)
  war#do_time_war;

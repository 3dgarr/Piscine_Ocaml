
let () = 
  let p1 = new People.people "Alice" in
  let dr = new Doctor.doctor "Doctor Who" 900 p1 in

  print_endline (dr#to_string);
  print_endline (dr#talk);
  print_endline (dr#use_sonic_screwdriver);
  print_endline (dr#regenerate_if_needed);
  dr#travel_in_time 2020 2050;
  dr#travel_in_time 2050 1800;
  dr#get_damage 50;
  print_endline (dr#regenerate_if_needed);
  dr#get_damage 60;
  print_endline (dr#regenerate_if_needed);
()
open App

let print_proj (name, status, grade) =
  Printf.printf "Project: %s | Status: %s | Grade: %d\n"
    name status grade

let () =
  let p1 = ("Piscine", "succeed", 90) in
  let p2 = (" OCaml", "failed", 70) in

  print_proj p1;
  print_proj p2;

  let combined = App.combine p1 p2 in
  print_proj combined;
  assert (combined = ("Piscine OCaml", "failed", 80));

  print_endline "---- Testing fail and success ----";

  let failed = App.fail p1 in
  print_proj failed;
  assert (failed = ("Piscine", "failed", 0));

  print_endline "--- ";

  let succeeded = App.success p2 in
  print_proj succeeded;
  assert (succeeded = (" OCaml", "succeed", 80));

  let zero_combined = App.combine p1 App.zero in
  print_proj zero_combined;
  assert (zero_combined = ("Piscine", "failed", 45));

  print_endline "All App tests passed"

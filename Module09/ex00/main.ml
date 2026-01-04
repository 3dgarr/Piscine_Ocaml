open Watchtower

let () =
  assert (Watchtower.add 5 Watchtower.zero = 5);
  assert (Watchtower.add Watchtower.zero 7 = 7);
  print_endline "Zero tests passed";

  assert (Watchtower.add 11 1 = 0);
  assert (Watchtower.add 10 5 = 3);
  print_endline "Add tests passed";

  assert (Watchtower.sub 5 3 = 2);
  assert (Watchtower.sub 3 5 = 10);
  print_endline "Sub tests passed";

  assert (Watchtower.add (-1) 1 = 0);
  assert (Watchtower.sub 0 1 = 11);
  print_endline "Negative number tests passed";

  assert (
    Watchtower.add (Watchtower.add 3 4) 5 =
    Watchtower.add 3 (Watchtower.add 4 5)
  );
  assert (
    Watchtower.sub (Watchtower.sub 10 3) 4 =
    Watchtower.sub 10 (Watchtower.add 3 4)
  );
  print_endline "Associativity tests passed";

  print_endline "All Watchtower tests passed"

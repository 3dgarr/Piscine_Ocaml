open Set.Set

let print_int_set s =
  print_string "{ ";
  foreach s (fun x -> Printf.printf "%d " x);
  print_endline "}"

let equal_set s1 s2 =
  for_all s1 (fun x -> exists s2 (fun y -> x = y))
  &&
  for_all s2 (fun x -> exists s1 (fun y -> x = y))

let () =
  let s1 = return 1 in
  assert (exists s1 (fun x -> x = 1));
  assert (for_all s1 (fun x -> x = 1));
  print_int_set s1;
  print_endline "Return test passed";

  let s2 = union (return 1 |> union (return 2)) (return 2 |> union (return 3)) in
  print_int_set s2;
  assert (equal_set s2 (union (return 1) (union (return 2) (return 3))));
  print_endline "Union test passed";

  let s3 = inter (union (return 1) (union (return 2) (return 3)))
                 (union (return 2) (union (return 3) (return 4))) in
  print_int_set s3;
  assert (equal_set s3 (union (return 2) (return 3)));
  print_endline "Inter test passed";


  let s4 = diff (union (return 1) (union (return 2) (return 3))) (return 2) in
  print_int_set s4;
  assert (exists s4 (fun x -> x = 1));
  assert (exists s4 (fun x -> x = 3));
  assert (not (exists s4 (fun x -> x = 2)));
  print_endline "Diff test passed";

  let s5 =
    filter
      (union (return 1) (union (return 2) (union (return 3) (return 4))))
      (fun x -> x mod 2 = 0)
  in
  print_int_set s5;
  assert (exists s5 (fun x -> x = 2));
  assert (exists s5 (fun x -> x = 4));
  assert (not (exists s5 (fun x -> x = 1)));

  print_endline "Filter test passed";

  let s6 =
    bind
      (union (return 1) (union (return 2) (return 3)))
      (fun x -> union (return x) (return (x * 10)))
  in
  print_int_set s6;
  assert (exists s6 (fun x -> x = 10));
  assert (exists s6 (fun x -> x = 30));
  print_endline "Bind test passed";

  assert (for_all (union (return 2) (union (return 4) (return 6))) (fun x -> x mod 2 = 0));
  assert (not (for_all (union (return 2) (union (return 3) (return 4))) (fun x -> x mod 2 = 0)));
  print_endline "For_all test passed";

  assert (exists (union (return 1) (union (return 3) (return 4))) (fun x -> x mod 2 = 0));
  assert (not (exists (union (return 1) (union (return 3) (return 5))) (fun x -> x mod 2 = 0)));
  print_endline "Exists test passed";

  print_endline "All Set monad tests passed"

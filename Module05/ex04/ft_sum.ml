let sum (a : float) (b : float) : float =
  a +. b

let () =
  assert (sum 1.0 2.0 = 3.0);
  assert (sum 3.14 2.86 = 6.0);
  assert (sum (-1.5) 4.2 = 2.7);
  assert (sum 0.0 0.0 = 0.0);
  assert (sum (-3.0) (-2.0) = -5.0);
  assert (sum 0.0 5.0 = 5.0);
  assert (sum 1e3 2.5e2 = 1250.0)

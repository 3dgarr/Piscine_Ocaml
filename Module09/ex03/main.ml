open Try.Try

let () =
  (* return *)
  let a = return 42 in
  assert (a = Success 42);

  (* bind success *)
  let b =
    bind (Success 10) (fun x -> Success (x * 2))
  in
  assert (b = Success 20);

  (* bind failure *)
  let c =
    bind (Failure (Failure "error")) (fun x -> Success (x * 2))
  in
  assert (c = Failure (Failure "error"));

  (* bind with exception *)
  let d =
    bind (Success 10) (fun _ -> failwith "boom")
  in
  (match d with
   | Failure _ -> ()
   | Success _ -> assert false);

  (* recover *)
  let e =
    recover (Failure (Failure "fail")) (fun _ -> Success 99)
  in
  assert (e = Success 99);

  (* recover does nothing on success *)
  let f =
    recover (Success 5) (fun _ -> Success 0)
  in
  assert (f = Success 5);

  (* filter success *)
  let g =
    filter (Success 10) (fun x -> x > 5)
  in
  assert (g = Success 10);

  (* filter failure *)
  let h =
    filter (Success 3) (fun x -> x > 5)
  in
  (match h with
   | Failure _ -> ()
   | Success _ -> assert false);

  (* flatten success *)
  let i =
    flatten (Success (Success 7))
  in
  assert (i = Success 7);

  (* flatten success of failure *)
  let j =
    flatten (Success (Failure (Failure "nested fail")))
  in
  (match j with
   | Failure _ -> ()
   | Success _ -> assert false);

  (* flatten failure *)
  let k =
    flatten (Failure (Failure "outer fail"))
  in
  (match k with
   | Failure _ -> ()
   | Success _ -> assert false);

  print_endline "All Try monad tests passed"

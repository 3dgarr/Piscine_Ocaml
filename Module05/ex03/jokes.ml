let () =
  let jokes = ref [||] in
  let rec read_lines ic =
    match input_line ic with
    | line -> jokes := Array.append !jokes [|line|]; read_lines ic
    | exception End_of_file -> ()
  in
  if Array.length Sys.argv > 1 then
    try
      let ic = open_in Sys.argv.(1) in
      (try
         read_lines ic
       with e ->
         close_in_noerr ic;
         prerr_endline ("Error reading file: " ^ Printexc.to_string e);
         exit 1);
      close_in ic
    with e ->
      prerr_endline ("Error opening file: " ^ Printexc.to_string e);
      exit 1
  else (
    prerr_endline "Usage: jokes <filename>";
    exit 1
  );
  Random.self_init ();
  if Array.length !jokes = 0 then (
    prerr_endline "No jokes found in file.";
    exit 1
  );
  let joke = !jokes.(Random.int (Array.length !jokes)) in
  print_endline joke;


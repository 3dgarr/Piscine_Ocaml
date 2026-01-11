let () =
  let jokes = ref [] in

  let rec read_lines ic =
    match input_line ic with
    | line ->
        jokes := line :: !jokes;
        read_lines ic
    | exception End_of_file -> ()
  in

  if Array.length Sys.argv > 1 then
    try
      let ic = open_in Sys.argv.(1) in
      (try
         read_lines ic
       with e ->
         close_in_noerr ic;
         print_endline ("Error reading file: " ^ Printexc.to_string e);
         exit 1);
      close_in ic
    with e ->
      print_endline ("Error opening file: " ^ Printexc.to_string e);
      exit 1
  else (
    print_endline "Usage: jokes <filename>";
    exit 1
  );

  Random.self_init ();

  if !jokes = [] then (
    print_endline "No jokes found in file.";
    exit 1
  );

  let jokes_array = Array.of_list !jokes in
  let joke = jokes_array.(Random.int (Array.length jokes_array)) in
  print_endline joke

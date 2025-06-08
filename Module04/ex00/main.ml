let () = 
  let colors = Color.all in
  List.iter (fun color ->
    Printf.printf "Verbose: %s, Short: %s\n" 
      (Color.toStringVerbose color) 
      (Color.toString color)
  ) colors;
  Printf.printf "Total colors: %d\n" (List.length colors)
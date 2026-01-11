let examples_of_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      let parts = String.split_on_char ',' line in
      let len = List.length parts in
      if len = 0 then read_lines acc else
      let floats =
        Array.of_list (List.map float_of_string (List.rev (List.tl (List.rev parts))))
      in
      let cls = List.nth parts (len - 1) in
      read_lines ((floats, cls) :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []


let () =
  (* let examples = examples_of_file "ionosphere.test.csv" in *)
  (* let examples = examples_of_file "ionosphere.train.csv" in *)
  let examples = examples_of_file "a.csv" in
  List.iter
    (fun (arr, cls) ->
       Array.iter (fun f -> Printf.printf "%f " f) arr;
       Printf.printf " -> %s\n" cls)
    examples


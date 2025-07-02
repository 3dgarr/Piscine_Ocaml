let my_sleep () = Unix.sleep 1

let main (argc:int) (argv:string array) = match argc with
  | n when n = 2 -> begin
    let seconds = 
      try
        int_of_string argv.(1)
      with Failure error -> raise (Invalid_argument "Error: Please Enter an Integer") in
      for i = seconds downto 0 do
        my_sleep ()
      done
    end
  | n when n < 2 -> raise (Invalid_argument "Error: Please provide an argument") 
  | _ -> raise (Invalid_argument "Error: Too many arguments")



let () =
  let argv = Sys.argv in
    try 
      main (Array.length argv) (argv) with 
      | Invalid_argument err_message -> print_endline err_message
      | _ -> ()
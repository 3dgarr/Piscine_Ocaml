(* let rec ft_countdown =  fun number -> if number > 0 then print_int number; print_endline "\n" ; ft_countdown(number - 1) ;; *)

  let rec ft_countdown number =
    if number >= 0 then 
      begin
        print_int number;
        print_char '\n';
        ft_countdown (number - 1)
      end
    else
      ()
  ;;


(* ft_countdown 100 ; *)
ft_countdown (-1) ;
ft_countdown 0;
(* ft_countdown (-1) *)
(* ft_countdown (-1) *)

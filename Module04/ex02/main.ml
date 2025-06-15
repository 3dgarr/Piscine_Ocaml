let () =
  let open Card in
    let all_cards = Card.all in
    let sorted_cards = List.sort Card.compare all_cards in
    let max_card = List.fold_left Card.max (List.hd sorted_cards) sorted_cards in
    let min_card = List.fold_left Card.min (List.hd sorted_cards) sorted_cards in

    Printf.printf "All cards:\n";
    List.iter (fun card -> Printf.printf "%s\n" (Card.toString card)) all_cards;

    Printf.printf "\nSorted cards:\n";
    List.iter (fun card -> Printf.printf "%s\n" (Card.toString card)) sorted_cards;

    Printf.printf "\nMax card: %s\n" (Card.toString max_card);
    Printf.printf "Min card: %s\n" (Card.toString min_card);
    
    Printf.printf "\nMax card verbose: %s\n" (Card.toStringVerbose max_card);
    Printf.printf "Min card verbose: %s\n" (Card.toStringVerbose min_card);
    Printf.printf "\nComparison of max and min card: %d\n" (Card.compare max_card min_card);
    Printf.printf "Best card in the list: %s\n" (Card.toString (Card.best sorted_cards));
    Printf.printf "Best card in the list (verbose): %s\n" (Card.toStringVerbose (Card.best sorted_cards));
    Printf.printf "Max card in the list: %s\n" (Card.toString (Card.max max_card min_card));
    Printf.printf "Min card in the list: %s\n" (Card.toString (Card.min max_card min_card));
    Printf.printf "Max card in the list (verbose): %s\n" (Card.toStringVerbose (Card.max max_card min_card));
    Printf.printf "Min card in the list (verbose): %s\n" (Card.toStringVerbose (Card.min max_card min_card));
    Printf.printf "Comparison of max and min card: %d\n" (Card.compare max_card min_card);
    print_endline "---------Testing isOf functions:-----------";
    Printf.printf "Is max card a Spade? %b\n" (Card.isSpade max_card);
    Printf.printf "Is min card a Spade? %b\n" (Card.isSpade min_card);
    print_endline "--------------------------------------------";
    Printf.printf "Is max card a Heart? %b\n" (Card.isHeart max_card);
    Printf.printf "Is min card a Heart? %b\n" (Card.isHeart min_card);
    print_endline "--------------------------------------------";
    Printf.printf "Is max card a Spade? %b\n" (Card.isSpade max_card);
    Printf.printf "Is min card a Spade? %b\n" (Card.isSpade min_card);
    print_endline "--------------------------------------------";
    Printf.printf "Is max card a Club? %b\n" (Card.isClub max_card);
    Printf.printf "Is min card a Club? %b\n" (Card.isClub min_card);
    print_endline "--------------------------------------------";
    Printf.printf "Is max card a Diamond? %b\n" (Card.isDiamond max_card);
    Printf.printf "Is min card a Diamond? %b\n" (Card.isDiamond min_card);
    
    print_endline "--------------------------------------------";

    print_endline "--------------------------------------------";
    Printf.printf "All Spades with both toString and toStringVerbose:\n";
    List.iter (fun card -> 
      Printf.printf "%s - %s\n" 
        (Card.toString card) 
        (Card.toStringVerbose card)
    ) Card.allSpades;
    print_endline "--------------------------------------------";
    Printf.printf "All Hearts with both toString and toStringVerbose:\n";
    List.iter (fun card -> 
      Printf.printf "%s - %s\n" 
        (Card.toString card) 
        (Card.toStringVerbose card)
    ) Card.allHearts;
    print_endline "--------------------------------------------";
    Printf.printf "All Diamonds with both toString and toStringVerbose:\n";
    List.iter (fun card -> 
      Printf.printf "%s - %s\n" 
        (Card.toString card) 
        (Card.toStringVerbose card)
    ) Card.allDiamonds;
    print_endline "--------------------------------------------";
    Printf.printf "All Clubs with both toString and toStringVerbose:\n";
    List.iter (fun card -> 
      Printf.printf "%s - %s\n" 
        (Card.toString card) 
        (Card.toStringVerbose card)
    ) Card.allClubs;

    print_endline "--------------------------------------------";
    print_endline "Testing of getValue and getColor functions";
    print_endline "--------------------------------------------";

    Printf.printf "Value of min card: %s\n" (Card.getValue min_card |> Card.Value.toStringVerbose);
    Printf.printf "Color of min card: %s\n" (Card.getColor min_card |> Card.Color.toStringVerbose);

    Printf.printf "Value of max card: %s\n" (Card.getValue max_card |> Card.Value.toStringVerbose);
    Printf.printf "Color of max card: %s\n" (Card.getColor max_card |> Card.Color.toStringVerbose);

    print_endline "--------------------------------------------";

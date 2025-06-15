module Card = Deck.Card
module Color = Card.Color
module Value = Card.Value

let print_card card =
  Printf.printf "Card: %s (%s)\n"
    (Card.toString card)
    (Card.toStringVerbose card)

let print_deck deck =
  let str_list = Deck.toStringList deck in
  Printf.printf "Deck (%d cards):\n" (List.length str_list);
  List.iter (Printf.printf "%s ") str_list;
  print_newline ()

let print_verbose_deck deck =
  let str_list = Deck.toStringListVerbose deck in
  Printf.printf "Verbose deck (%d cards):\n" (List.length str_list);
  List.iter (Printf.printf "%s\n") str_list

let test_deck_creation () =
  Printf.printf "Test 1: Creating a new deck...\n";
  let deck = Deck.newDeck () in
  assert (List.length (Deck.toStringList deck) = 52);

  Printf.printf "Deck has %d cards.\n" (List.length (Deck.toStringList deck));
  print_deck deck;
  print_endline "Deck creation test passed!\n";
  deck

let test_draw_card deck =
  Printf.printf "Test 2: Drawing a card...\n";
  let (card, new_deck) = Deck.drawCard deck in
  print_card card;
  Printf.printf "Remaining cards in deck: %d\n" (List.length (Deck.toStringList new_deck));
  assert (List.length (Deck.toStringList new_deck) = List.length (Deck.toStringList deck) - 1);
  print_endline "Draw card test passed!\n";
  new_deck

let test_deck_toString deck =
  Printf.printf "Test 3: Deck toStringList...\n";
  let strs = Deck.toStringList deck in
  assert (List.length strs = List.length (Deck.toStringList deck));
  List.iter (Printf.printf "%s " ) strs;
  print_newline ();
  print_endline "Deck toStringList test passed!\n"

let test_deck_toStringVerbose deck =
  Printf.printf "Test 4: Deck toStringListVerbose...\n";
  let strs = Deck.toStringListVerbose deck in
  assert (List.length strs = List.length (Deck.toStringList deck));
  List.iter (Printf.printf "%s\n" ) strs;
  print_endline "Deck toStringListVerbose test passed!\n"

let () =
  Random.self_init ();
  let deck = test_deck_creation () in
  let deck_after_draw = test_draw_card deck in
  test_deck_toString deck_after_draw;
  test_deck_toStringVerbose deck_after_draw;
  print_endline "✅ All Deck module tests completed successfully!"

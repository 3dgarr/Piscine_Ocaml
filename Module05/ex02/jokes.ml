let () =
  let jokes = [|
    "What's missing in an orphanage computer? The motherboard.";
    "Why do dwarfs work at Lidl (Market)?? Because dwarfs are really lidl.";
    "I won't ever joke about dwarfs..I'm a responsible man, they look up to me.";
    "What's blue and smells like red paint? Blue paint.";
    "Why did the math book look sad? Because it had too many problems.";
    "What's the best thing about Switzerland? I don't know, but the flag is a big plus."
  |] in
  Random.self_init ();
  let joke = jokes.(Random.int (Array.length jokes)) in
  print_endline joke;
class doctor name age (sidekick : People.people) =
object (self)

    (* attributes *)
    val mutable __name : string = name
    val mutable __age : int = age
    val mutable __sidekick : People.people = sidekick
    val mutable __hp : int = 100

    (* to_string method *)
    method to_string : string =
        "Doctor: " ^ __name ^
        "\nAge: " ^ string_of_int __age ^
        "\nHP: " ^ string_of_int __hp ^
        "\nSidekick: (" ^ __sidekick#to_string ^ ")";

    (* talk method *)
    method talk : string = 
      "Hi, I'm a Doctor!"

    (* private regenerate method *)
    method private regenerate : string =
        __hp <- 100;
      __name ^ " has regenerated and restored HP to " ^ string_of_int __hp

    (* regeneration check *)
    method regenerate_if_needed : string =
      if __hp <= 0 then
        self#regenerate
      else
        __name ^ " does not need to regenerate. Current HP: " ^
                       string_of_int __hp
    
    method get_damage (damage:int) : unit =
      if damage < 0 then
        ()
      else
      if damage >= __hp then
        __hp <- 0
      else
        __hp <- __hp - damage

    method attack (d : Dalek.dalek) : unit =
      print_endline (__name ^ " attacks the Dalek: " ^ self#use_sonic_screwdriver ^ "!");
      d#get_damage 20


    (* sonic screwdriver *)
    method use_sonic_screwdriver : string =
      "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

    (* private: draw TARDIS *)
    method private draw_tardis : unit =
      let tardis = [
        "          _____________";
        "         |   POLICE    |";
        "         |   PUBLIC    |";
        "         |    CALL     |";
        "   ______|_____________|_____";
        "  |    ____   ____   ____    |";
        "  |   |####| |####| |####|   |";
        "  |   |####| |####| |####|   |";
        "  |    ----   ----   ----    |";
        "  |__________________________|";
        "  |                          |";
        "  |   [ ]      [ ]      [ ]  |";
        "  |   [ ]      [ ]      [ ]  |";
        "  |                          |";
        "  |   [ ]      [ ]      [ ]  |";
        "  |   [ ]      [ ]      [ ]  |";
        "  |__________________________|";
        "  |                          |";
        "  |__________________________|";
      ] in
      List.iter print_endline tardis

    (* travel_in_time method *)
    method travel_in_time (start:int) (arrival:int) : unit =
      print_endline ("Traveling from " ^ string_of_int start ^
                     " to " ^ string_of_int arrival ^ "!");
      __age <- __age + (arrival - start);
      print_endline (__name ^ " has aged to " ^ string_of_int __age ^ " years old.");
      self#draw_tardis

    (* initializer *)
    initializer
        print_endline ("A new doctor has entered the game: " ^ __name)

end

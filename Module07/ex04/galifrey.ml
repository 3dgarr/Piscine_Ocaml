class galifrey =
object (self)

  (* ------------ MEMBERS ------------ *)
  val mutable daleks  : Dalek.dalek list  = []
  val mutable doctors : Doctor.doctor list = []
  val mutable people  : People.people list = []

  (* ------------ ADD METHODS ------------ *)
  method add_dalek d   = daleks  <- d :: daleks
  method add_doctor d  = doctors <- d :: doctors
  method add_people p  = people  <- p :: people

  (* ------------ CHECK IF ANYONE ALIVE ------------ *)
  method private any_dalek_alive =
    List.exists (fun d -> d#get_hp > 0) daleks

  method private any_doctor_alive =
    List.exists (fun d -> d#get_hp > 0) doctors

  method private any_people_alive =
    List.exists (fun p -> true) people   (* people have no HP so they always die instantly *)

  (* ------------ REMOVE DEAD ------------ *)
  method private remove_dead =
    daleks  <- List.filter (fun d -> d#get_hp > 0) daleks;
    doctors <- List.filter (fun d -> d#get_hp > 0) doctors;
    people  <- List.filter (fun _ -> false) people  (* all people die instantly *)

  (* ------------ ONE ROUND OF FIGHT ------------ *)
  method private daleks_attack =
    List.iter (fun d ->
      if d#get_hp > 0 then (
        (* Priority: attack doctor first, then people *)
        match doctors with
        | doc :: _ when doc#get_hp > 0 ->
            print_endline ("Dalek " ^ d#get_name ^ " attacks Doctor!");
            doc#get_damage 20
        | _ ->
            (match people with
             | p :: _ ->
                print_endline ("Dalek " ^ d#get_name ^ " exterminates a human!");
                ignore (p#die)
             | [] -> ())
      )
    ) daleks

  method private doctors_attack =
    List.iter (fun doc ->
      if doc#get_hp > 0 then
      match daleks with
      | d :: _ ->
          print_endline ("Doctor attacks Dalek! Sonic Boom!");
          d#get_damage 30
      | [] -> ()
    ) doctors

  (* ------------ TIME WAR ------------ *)
  method do_time_war =
    print_endline "⚡ TIME WAR BEGINS ⚡";

    let rec loop round =
      print_endline ("\n--- ROUND " ^ string_of_int round ^ " ---");

      self#daleks_attack;
      self#doctors_attack;
      self#remove_dead;

      if not self#any_dalek_alive then
        print_endline "\n✨ Time Lords Win! Daleks are destroyed!"
      else if not self#any_doctor_alive then
        print_endline "\n💀 Daleks Win! No Time Lords remain!"
      else
        loop (round + 1)
    in
    loop 1

end

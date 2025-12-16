class virtual atom (name : string) (symbol : string) (atomic_number : int) =
  object (self)
    method name : string = name
    method symbol : string = symbol
    method atomic_number : int = atomic_number

    method to_string : string = "Atom: " ^ self#name ^ " (" ^ self#symbol ^ "), Atomic Number: " ^ string_of_int self#atomic_number

    method equals (other : atom) : bool =
      self#name = other#name &&
      self#symbol = other#symbol &&
      self#atomic_number = other#atomic_number
  end


  type atom_ctor = unit -> atom

  let make_atoms_list (ctor : atom_ctor) (n : int) : atom list =
    let rec aux acc i =
      if i <= 0 then acc
      else aux (ctor () :: acc) (i - 1)
    in
    aux [] n
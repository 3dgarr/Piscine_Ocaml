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

class hydrogen =
  object
    inherit atom "Hydrogen" "H" 1
  end

class carbon =
  object
    inherit atom "Carbon" "C" 6
  end

class oxygen =
  object
    inherit atom "Oxygen" "O" 8
  end

class chlorine =
  object
    inherit atom "Chlorine" "Cl" 17
  end

class oganesson =
  object
    inherit atom "Oganesson" "Og" 118
  end

class molybdenum =
  object
    inherit atom "Molybdenum" "Mo" 42
  end

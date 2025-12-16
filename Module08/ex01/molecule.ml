open Atom

class virtual molecule (name : string) (atoms : atom list) =
  object (self)
    method name : string = name
    
    method formula : string =
      self#atom_count
      |> self#hill_sort
      |> List.map (fun (s, n) ->
          if n = 1 then s else s ^ string_of_int n)
      |> String.concat ""
    
    method private atoms : atom list = atoms


    method private atom_count : (string * int) list =
      let add_atom acc atom =
        let symbol = atom#symbol in
        if List.mem_assoc symbol acc then
          List.map
            (fun (s, c) -> if s = symbol then (s, c + 1) else (s, c))
            acc
        else
          (symbol, 1) :: acc
      in
      List.fold_left add_atom [] atoms


    method private hill_sort (lst : (string * int) list) =
      let has_carbon = List.mem_assoc "C" lst in
      let cmp (a, _) (b, _) = String.compare a b in
      if has_carbon then
        let c = List.filter (fun (s, _) -> s = "C") lst in
        let h = List.filter (fun (s, _) -> s = "H") lst in
        let rest =
          List.filter (fun (s, _) -> s <> "C" && s <> "H") lst
          |> List.sort cmp
        in
        c @ h @ rest
      else
        List.sort cmp lst


    method to_string : string =
      self#name ^ ": " ^ self#formula


    method equals (other : molecule) : bool =
      self#formula = other#formula

      
  end
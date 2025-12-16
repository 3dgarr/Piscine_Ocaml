open Molecules

class virtual reaction
  (start : (molecule * int) list)
  (result : (molecule * int) list) =
object (self)

  method get_start : (molecule * int) list = start
  method get_result : (molecule * int) list = result

  method private count_atoms (side : (molecule * int) list) :
    (string * int) list =

    let add_atom acc (symbol, count) =
      let prev = try List.assoc symbol acc with Not_found -> 0 in
      (symbol, prev + count) :: List.remove_assoc symbol acc
    in

    let atoms =
      List.fold_left
        (fun acc (mol, coeff) ->
           List.fold_left
             (fun acc (symbol, count) ->
                add_atom acc (symbol, count * coeff))
             acc
             mol#atoms)
        []
        side
    in
    List.sort compare atoms

  (* Lavoisier law *)
  (* "Nothing is lost, nothing is created, everything is transformed." *)
  method is_balanced : bool =
    self#count_atoms start = self#count_atoms result

  (* Balancing is reaction-specific *)
  method virtual balance : reaction

end

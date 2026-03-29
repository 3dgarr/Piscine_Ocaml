open Molecule

class virtual reaction (start:(molecule * int) list) (result:(molecule * int) list) =
object (this)
    method virtual get_start : (molecule * int) list
    method virtual get_result : (molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool
    method virtual to_string : string

end
type t = Spade | Heart | Diamond | Club

let toStringVerbose = function
  | Spade -> "Spade"
  | Heart -> "Heart"
  | Diamond -> "Diamond"
  | Club -> "Club"

let toString = function
  | Spade -> "S"
  | Heart -> "H"
  | Diamond -> "D"
  | Club -> "C"

let all = [Spade; Heart; Diamond; Club]
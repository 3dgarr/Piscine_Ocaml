module Color = struct
  type t = Spade | Heart | Diamond | Club

  let toString = function
    | Spade -> "S"
    | Heart -> "H"
    | Diamond -> "D"
    | Club -> "C"

  let toStringVerbose = function
    | Spade -> "Spade"
    | Heart -> "Heart"
    | Diamond -> "Diamond"
    | Club -> "Club"

  let all = [Spade; Heart; Diamond; Club]

end

module Value = struct

  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

  let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As] 

  let toInt = function
    | T2 -> 1
    | T3 -> 2
    | T4 -> 3
    | T5 -> 4
    | T6 -> 5
    | T7 -> 6
    | T8 -> 7
    | T9 -> 8
    | T10 -> 9
    | Jack -> 10
    | Queen -> 11
    | King -> 12
    | As -> 13

  let toString = function
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"


  let toStringVerbose = function
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | As -> "As"

  let next = function
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> As
    | As -> invalid_arg "No next value for As"

  let previous = function
    | T2 -> invalid_arg "No previous value for T2"
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen -> Jack
    | King -> Queen
    | As -> King

  end

type t = { color: Color.t; value: Value.t }

let newCard (color: Color.t) (value: Value.t) : t =
  { color; value }


let allSpades : t list = 
  List.map (fun v -> newCard Color.Spade v) Value.all

let allHearts : t list =
  List.map (fun v -> newCard Color.Heart v) Value.all
let allDiamonds : t list =
  List.map (fun v -> newCard Color.Diamond v) Value.all
let allClubs : t list =
  List.map (fun v -> newCard Color.Club v) Value.all

let all : t list =

  List.flatten [
    allSpades;
    allHearts;
    allDiamonds;
    allClubs;
  ]

let getValue (card: t) : Value.t =
  card.value

let getColor (card: t) : Color.t =
  card.color

let toString (card: t) : string =
  Printf.sprintf "%s%s" (Value.toString card.value) (Color.toString card.color)

let toStringVerbose (card: t) : string =
  Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose card.value) (Color.toStringVerbose card.color)

let compare (card1: t) (card2: t) : int =
  let color_comparison = compare card1.color card2.color in
  if color_comparison <> 0 then
    color_comparison
  else
    compare card1.value card2.value


let max (card1: t) (card2: t) : t =
  if compare card1 card2 >= 0 then
    card1
  else
    card2

let min (card1: t) (card2: t) : t =
  if compare card1 card2 <= 0 then
    card1
  else
    card2

let best (cards: t list) : t = 
  match cards with
  | [] -> invalid_arg "best: empty list"
  | h :: t ->
    List.fold_left (fun acc card ->
      if compare card acc > 0 then card else acc
    ) h t

let isOf (card: t) (color: Color.t) : bool =
  card.color = color

let isSpade (card: t) : bool =
  isOf card Color.Spade

let isHeart (card: t) : bool =
  isOf card Color.Heart

let isDiamond (card: t) : bool =
  isOf card Color.Diamond

let isClub (card: t) : bool =
  isOf card Color.Club

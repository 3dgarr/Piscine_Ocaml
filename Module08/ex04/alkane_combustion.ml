open Molecule
open Molecules
open Alkane
open Reaction

exception Unbalanced

let rec gcd a b =
  let a = abs a and b = abs b in
  if b = 0 then a else gcd b (a mod b)

let gcd_list = function
  | [] -> 1
  | x :: xs -> List.fold_left gcd x xs

let add_counts acc (sym, n) =
  let rec aux = function
    | [] -> [ (sym, n) ]
    | (s, c) :: t when s = sym -> (s, c + n) :: t
    | h :: t -> h :: aux t
  in
  aux acc

let sum_atoms pairs =
  List.fold_left
    (fun acc (m, coeff) ->
      List.fold_left
        (fun a (s, k) -> add_counts a  (s, k * coeff) )
        acc m#atom_count)
    [] pairs

let atom_maps_equal a b =
  let sort = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) in
  sort a = sort b

let is_balanced_start_result start result =
  atom_maps_equal (sum_atoms start) (sum_atoms result)

let merge_alkanes alkanes =
  List.fold_left
    (fun acc (a : #alkane) ->
      let m = (a :> molecule) in
      let key = m#formula in
      let rec upd = function
        | [] -> [ (m, 1) ]
        | (mm, c) :: t when mm#formula = key -> (mm, c + 1) :: t
        | h :: t -> h :: upd t
      in
      upd acc)
    [] alkanes

let carbon_count (m : #molecule) =
  try List.assoc "C" m#atom_count with Not_found -> 0

let balanced_start_result merged =
  let o2 = new oxygen_molecule in
  let co2 = new carbon_dioxide in
  let h2o = new water in
  let n_list =
    List.map (fun (m, w) -> (m, w, carbon_count m)) merged
  in
  let wsum =
    List.fold_left (fun acc (_, w, n) -> acc + w * (3 * n + 1)) 0 n_list
  in
  let alpha = if wsum mod 2 = 0 then 1 else 2 in
  let scaled =
    List.map (fun (m, w, n) -> (m, w * alpha, n)) n_list
  in
  let b =
    List.fold_left (fun acc (_, a, n) -> acc + a * (3 * n + 1)) 0 scaled / 2
  in
  let co2_n =
    List.fold_left (fun acc (_, a, n) -> acc + a * n) 0 scaled
  in
  let h2o_n =
    List.fold_left (fun acc (_, a, n) -> acc + a * (n + 1)) 0 scaled
  in
  let alk_parts = List.map (fun (m, a, _) -> (m, a)) scaled in
  let all_coeffs =
    b :: co2_n :: h2o_n :: List.map snd alk_parts
  in
  let g = gcd_list all_coeffs in
  let alk_final = List.map (fun (m, a) -> (m, a / g)) alk_parts in
  let b' = b / g and co2' = co2_n / g and h2o' = h2o_n / g in
  (alk_final @ [ (o2, b') ], [ (co2, co2'); (h2o, h2o') ])

let fmt_term (m, c) =
  if c = 1 then m#formula
  else string_of_int c ^ " " ^ m#formula

let equation_string start result =
  String.concat " + " (List.map fmt_term start)
  ^ " -> "
  ^ String.concat " + " (List.map fmt_term result)

class alkane_combustion alkanes =
  let merged =
    let m = merge_alkanes alkanes in
    if List.length m = 0 then invalid_arg "alkane_combustion: empty alkane list" else m
  in
  let o2 = new oxygen_molecule in
  let co2 = new carbon_dioxide in
  let h2o = new water in
  let start_ub = merged @ [ (o2, 1) ] in
  let result_ub = [ (co2, 1); (h2o, 1) ] in
  object (self)
    inherit reaction start_ub result_ub

    method get_start =
      if self#is_balanced then start_ub else raise Unbalanced

    method get_result =
      if self#is_balanced then result_ub else raise Unbalanced

    method balance =
      let st, res = balanced_start_result merged in
      (new alkane_combustion_balanced merged st res :> reaction)

    method is_balanced = is_balanced_start_result start_ub result_ub

    method to_string = equation_string start_ub result_ub
  end

and alkane_combustion_balanced merged st res =
  object (self)
    inherit reaction st res

    method get_start =
      if self#is_balanced then st else raise Unbalanced

    method get_result =
      if self#is_balanced then res else raise Unbalanced

    method balance =
      let st', res' = balanced_start_result merged in
      (new alkane_combustion_balanced merged st' res' :> reaction)

    method is_balanced = is_balanced_start_result st res

    method to_string = equation_string st res
  end
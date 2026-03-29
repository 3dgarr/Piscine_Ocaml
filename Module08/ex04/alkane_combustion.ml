open Atom
open Atoms
open Molecule
open Molecules
open Reaction

exception NotBalanced

class alkane_combustion (alkanes:(molecule * int) list) =
object (self)
  inherit reaction [] []

  (* Deduplicate molecules by formula and sum coefficients *)
  method private alkanes_dedup =
    let tbl = Hashtbl.create 10 in
    List.iter (fun (a,c) ->
      match Hashtbl.find_opt tbl a#formula with
      | None -> Hashtbl.add tbl a#formula (a,c)
      | Some (prev_a, prev_c) -> Hashtbl.replace tbl a#formula (prev_a, prev_c + c)
    ) alkanes;
    Hashtbl.fold (fun _ v acc -> v::acc) tbl []

  (* Total number of C atoms *)
  method private total_c =
    List.fold_left (fun acc (a,cnt) ->
      let c_count = try List.assoc "C" (a#atom_count) with Not_found -> 0 in
      acc + c_count * cnt
    ) 0 (self#alkanes_dedup)

  (* Total number of H atoms *)
  method private total_h =
    List.fold_left (fun acc (a,cnt) ->
      let h_count = try List.assoc "H" (a#atom_count) with Not_found -> 0 in
      acc + h_count * cnt
    ) 0 (self#alkanes_dedup)

  (* Determine multiplier to avoid fractions *)
  method private multiplier =
    if (2*self#total_c + (self#total_h / 2)) mod 2 <> 0 then 2 else 1

  (* CO2 coefficient *)
  method private co2_count =
    self#total_c * self#multiplier

  (* H2O coefficient *)
  method private h2o_count =
    (self#total_h / 2) * self#multiplier

  (* O2 coefficient *)
  method private o2_count =
    ((2*self#total_c + (self#total_h / 2)) / 2) * self#multiplier

  (* Get start molecules (alkanes + O2) *)
  method get_start =
    if not (self#is_balanced) then raise NotBalanced
    else
      (self#alkanes_dedup) @ [(new oxygen_molecule, self#o2_count)]

  (* Get result molecules (CO2 + H2O) *)
  method get_result =
    if not (self#is_balanced) then raise NotBalanced
    else
      [(new carbon_dioxide, self#co2_count); (new water, self#h2o_count)]

  (* Return a new balanced combustion reaction *)
  method balance = new alkane_combustion (self#alkanes_dedup)

  (* Check if reaction is balanced *)
  method is_balanced =
    let o_atoms_start = self#o2_count * 2 in
    let o_atoms_end = 2*self#co2_count + self#h2o_count in
    self#total_c * self#multiplier = self#co2_count &&
    self#total_h * self#multiplier = self#h2o_count * 2 &&
    o_atoms_start = o_atoms_end

  (* Convert reaction to string *)
  method to_string : string =
    let side_to_string lst =
      lst
      |> List.map (fun (m,c) ->
          if c = 1 then m#name
          else Printf.sprintf "%d %s" c m#name)
      |> String.concat " + "
    in
    Printf.sprintf "%s -> %s" (side_to_string self#get_start) (side_to_string self#get_result)
end
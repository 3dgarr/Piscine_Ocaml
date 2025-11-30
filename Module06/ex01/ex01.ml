module OwnHash = struct
(* Implementation of a Fowler–Noll–Vo (FNV-1a) *)
  type t = string

  let equal = String.equal

  let hash s =
    let h = ref 0 in
    for i = 0 to String.length s - 1 do
      h := (!h * 31) + Char.code s.[i]
    done;
    !h land max_int
end


module StringHashtbl = Hashtbl.Make(OwnHash)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
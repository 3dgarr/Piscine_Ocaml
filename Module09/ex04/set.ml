module Set =
struct
  type 'a t = 'a list

  let mem x s =
    List.exists (fun y -> x = y) s

  let add x s =
    if mem x s then s else x :: s

  let return x =
    [x]

  let union s1 s2 =
    List.fold_left (fun acc x -> add x acc) s1 s2

  let inter s1 s2 =
    List.filter (fun x -> mem x s2) s1

  let diff s1 s2 =
    List.filter (fun x -> not (mem x s2)) s1

  let bind s f =
    List.fold_left
      (fun acc x -> union acc (f x))
      []
      s

  let filter s p =
    List.filter p s

  let foreach s f =
    List.iter f s

  let for_all s p =
    List.for_all p s

  let exists s p =
    List.exists p s
end

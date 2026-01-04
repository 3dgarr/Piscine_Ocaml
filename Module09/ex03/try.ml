module Try =
struct
  type 'a t =
    | Success of 'a
    | Failure of exn

  let return x =
    Success x

  let bind m f =
    match m with
    | Failure e -> Failure e
    | Success x ->
        try
          f x
        with e ->
          Failure e

  let recover m f =
    match m with
    | Success _ -> m
    | Failure e -> f e

  let filter m p =
    match m with
    | Failure e -> Failure e
    | Success x ->
        try
          if p x then Success x
          else Failure (Failure "Predicate failed")
        with e ->
          Failure e

  let flatten m =
    match m with
    | Failure e -> Failure e
    | Success inner ->
        match inner with
        | Success x -> Success x
        | Failure e -> Failure e
end

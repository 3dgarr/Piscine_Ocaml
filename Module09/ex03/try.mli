module Try :
sig
  type 'a t =
    | Success of 'a
    | Failure of exn

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val recover : 'a t -> (exn -> 'a t) -> 'a t
  val filter : 'a t -> ('a -> bool) -> 'a t
  val flatten : 'a t t -> 'a t
end

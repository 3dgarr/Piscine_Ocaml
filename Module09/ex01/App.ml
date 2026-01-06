module App :
sig
  type project = string * string * int
  val zero : project
  val combine : project -> project -> project
  val fail : project -> project
  val success : project -> project
end
=
struct
  type project = string * string * int

  let zero : project = ("", "", 0)

  let combine (name1, _, grade1) (name2, _, grade2) : project =
    let combined_name = name1 ^ name2 in
    let average_grade = (grade1 + grade2) / 2 in
    let status = if average_grade > 80 then "succeed" else "failed" in
    (combined_name, status, average_grade)

  let fail (name, _, _) : project =
    (name, "failed", 0)

  let success (name, _, _) : project =
    (name, "succeed", 80)
end
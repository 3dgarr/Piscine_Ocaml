(* Your monoid will contain:
• A type project as a product type consisting of a string, a string representing
status (either "fail" or "succeed"), and an integer for the grade.
• A zero, which consists of two empty strings and a 0.
• A combine rule to combine two projects, resulting in a new project with
the strings concatenated and the average of the grades. If the average
is above 80, the status is "succeed"; otherwise, it’s "failed".
• A fail rule to create a new project from the project passed as a parameter,
with a grade equal to 0 and status set to "failed".
• A success rule to create a new project from the project passed as a parameter,
with a grade of 80 and status set to "succeed".
• Additionally, you will provide a print_proj function in your main for
testing purposes, typed as App.project -> unit. *)

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
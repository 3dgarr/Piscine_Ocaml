type phosphate = string
type deoxyribose = string
type nucleobase = 
  | A
  | T
  | C
  | G
  | None

type nucleotide = {
  phosphate: phosphate;
  deoxyribose: deoxyribose;
  nucleaobase: nucleobase;
}

let generate_nucleotide (c:nucleobase) : nucleotide =
  {
    phosphate = "phosphate";
    deoxyribose = "deoxyribose";
    nucleaobase = match c with
      | A -> A
      | T -> T
      | C -> C
      | G -> G
      | None -> None
  }

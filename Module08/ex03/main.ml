open Molecules
open Alkane
open Alkanes


let () =
  let m = new methane in
  let e = new ethane in
  let p = new propane in
  let b = new butane in
  let pe = new pentane in
  let h = new hexane in
  let he = new heptane in
  let o = new octane in
  let n = new nonane in
  let d = new decane in
  let u = new undecane in
  let doo = new dodecane in
  
  List.iter
  (fun a -> print_endline a#to_string)
  [ m; e; p; b; pe; h; he; o; n; d; u; doo];
  
  print_endline ("Trying unsupported alkane:");
  try
    let _ = new unsupported_alkane in
    ()
  with Invalid_argument msg ->
    print_endline ("Caught exception: " ^ msg) 
  

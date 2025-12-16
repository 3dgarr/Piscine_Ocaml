open Atom
open Atoms
open Molecule

class water =
  object
    inherit molecule "Water"
      ( 
        make_atoms_list (fun () -> new hydrogen) 2
      @ make_atoms_list (fun () -> new oxygen) 1 
      )
  end

class carbon_dioxide =
  object
    inherit molecule "Carbon dioxide"
    ( 
      make_atoms_list (fun () -> new carbon) 1
    @ make_atoms_list (fun () -> new oxygen) 2 
    )
  end

class methane =
  object
    inherit molecule "Methane"
      ( 
        make_atoms_list (fun () -> new carbon) 1
      @ make_atoms_list (fun () -> new hydrogen) 4 
      )
  end

class ammonia =
  object
    inherit molecule "Ammonia"
      ( 
        make_atoms_list (fun () -> new nitrogen) 1
      @ make_atoms_list (fun () -> new hydrogen) 3 
      )
  end

class ethanol =
  object
    inherit molecule "Ethanol"
      ( 
        make_atoms_list (fun () -> new carbon) 2
      @ make_atoms_list (fun () -> new hydrogen) 6
      @ make_atoms_list (fun () -> new oxygen) 1 
      )
  end


class trinitrotoluene =
  object
    inherit molecule "Trinitrotoluene"
      ( 
        make_atoms_list (fun () -> new nitrogen) 3
      @ make_atoms_list (fun () -> new hydrogen) 5
      @ make_atoms_list (fun () -> new oxygen) 6
      @ make_atoms_list (fun () -> new carbon) 7 
      )
  end
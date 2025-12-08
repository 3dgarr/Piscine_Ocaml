class people name =
object
  val mutable __name : string = name
  val mutable __hp : int = 100
  
  
  method to_string : string =
    "Name: " ^ __name ^ ", HP: " ^ string_of_int __hp
    
  method talk : string =
    "I'm " ^ __name ^ "! Do you know the Doctor?"
      
  method die : string =
    "Aaaarghh!"
        
  initializer
    print_endline ("A new person has entered the game: " ^ __name)

end
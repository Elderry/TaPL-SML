signature LAMBDA =
sig

    structure Type: 
              sig    
                  datatype t
                    = Bool
                    | Var of string
                    | Arrow of t * t
              end
              
    datatype t
      = True
      | False
      | If of t * t * t
      | Var of string
      | Abs of string * t
      | App of t * t
               
    val pp: t -> unit
    val typeCheck: t -> Type.t
                        
end

signature LET = sig

    structure Type: sig    

        datatype t =
	        Bool
            | Var of string
            | Arrow of t * t
            | Pair of t * t
                   
        datatype scheme = Forall of string list * t

    end
              
    datatype t =
    	True
      	| False
      	| If of t * t * t
      	| Var of string
      	| Abs of string * t
      	| App of t * t
      	| Let of string * t * t
      	| Pair of t * t
               
    val pp: t -> unit
    val typeCheck: t -> Type.t
                        
end
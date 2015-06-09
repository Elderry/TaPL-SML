signature REFERENCE = sig

    structure Type: sig
  		datatype t
    		= Bool
            | Unit
            | Fun of t * t
            | Ref of t
                         
        val equals: t * t -> bool
        val toString: t -> string
    end

    datatype t
    	= True
      	| False
      	| If of t * t * t
      	| Var of string
      	| Abs of string * Type.t * t
      	| App of t * t
      	| Ref of t
      	| Deref of t
      	| Assign of t * t
      	| Address of string
      	| Unit

    val eval: t -> t
    val evalAll: t -> t
    val pp: t -> unit
    val typeCheck: t -> Type.t

end
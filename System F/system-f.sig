signature SYSTEMF = sig

    structure Type:sig

        datatype t =
            Bool
            | Var of string
            | Arrow of t * t
            | Product of t * t
            | Forall of string * t
            | Exists of string * t

        val equals: t * t -> bool
        val pp: t -> unit

    end

    datatype t =
        True
        | False
        | If of t * t * t
        | Var of string
        | Abs of string * Type.t * t
        | App of t * t
        | Pair of t * t
        | First of t
        | Second of t
        | TyAbs of string * t
        | TyApp of t * Type.t
        | Pack of Type.t * t * Type.t
        | Unpack of string * string * t * t
               
    val pp: t -> unit
    val typeCheck: t -> Type.t
                        
end
signature SYSTEMF = sig

    (* T -> Bool
     *    | X
     *    | T -> T
     *    | Forall X.T
     *    | Exists X.T
     *)
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

    (* e -> true
     *    | false
     *    | if (e, e, e)
     *    | x
     *    | lambda x:T.e
     *    | e e
     *    | (e, e)
     *    | #1 e
     *    | #2 e
     *    | TyLambda X.e
     *    | e [T]
     *    | pack {T, e} as T'
     *    | unpack {X, x}=e1 in e2
     *)
              
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
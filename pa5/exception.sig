signature EXCEPTION = sig

    structure Type:sig
        datatype t =
            Any
            | Int
        val equals: t * t -> bool
    end

    datatype t =
        Num of int
        | Add of t * t
        | Throw
        | Try of t * t

    val evalAll: t -> t
    val pp: t -> unit
    val typeCheck: t -> Type.t

end
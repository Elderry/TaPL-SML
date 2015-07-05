signature TYPE = sig

    datatype ty
      = Bool
      | Fun of ty * ty

    datatype t
      = True
      | False
      | If of t * t * t
      | Var of string
      | Abs of string * ty * t
      | App of t * t

    val tyEquals: ty * ty -> bool
    val typeCheck: t -> ty

end


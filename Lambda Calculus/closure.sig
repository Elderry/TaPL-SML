signature CLOSURE = sig

    datatype t
      = Num of int
      | Var of string
      | Abs of string * t
      | App of t * t
(* the implicit invariant here is that
 * the second "t" should always be a lambda
 *)
      | Closure of env * t
         and env 
           = T of string ->t 

    val emptyEnv: env
    val eval: env * t -> env * t
    val evalAll: env * t -> env * t
    val pp: t -> unit

end
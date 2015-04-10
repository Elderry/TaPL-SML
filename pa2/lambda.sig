signature LAMBDA = sig
    (* We start by defining a syntax for the
     * standard lambda-calculus in chapter 5.
     *)
    datatype t 
      = Var of string
      | Abs of string * t
      | App of t * t

    (* small-step operational semantics *)
    val eval: t -> t
    (* multi-step evaluator *)
    val evalAll: t -> t
    (* a pretty printer *)
    val pp: t -> unit
end

signature ARITH = sig

    datatype t =
        True
        | False
        | If of t * t * t
        | Zero
        | Succ of t
        | Pred of t
        | IsZero of t
                  
    (* small-step evaluator *)
    val eval: t -> t
    (* multi-step evaluator *)
    val evalAll: t -> t
    (* big-step evaluator *)
    val evalBig: t -> t
    (* a pretty printer *)
    val pp: t -> unit

end

structure Arith: ARITH = struct

    datatype t =
        True
        | False
        | If of t * t * t
        | Zero
        | Succ of t
        | Pred of t
        | IsZero of t

    fun isNumber t = case t
        of Zero => true
        | Succ t' => isNumber t'
        | _ => false

    fun isValue t = if isNumber t
                    then true
                    else case t of
            			True => true
            			| False => true
            			| _ => false
             
    exception NoRule
          
    (* one-step evaluator *)
    fun eval t = case t of
        If (True, t2, _) => t2
        | If (False, _, t3) => t3
        | If (t1, t2, t3) => If (eval t1, t2, t3)
        | Succ t' => Succ (eval t')
        | Pred Zero => Zero
        | Pred (Succ t') => if isNumber t'
                            then t'
                            else Pred (eval (Succ t'))
        | Pred t' => Pred (eval t')
        | IsZero Zero => True
        | IsZero (Succ t') => if isNumber t'
                            then False
                            else IsZero (eval (Succ t'))
        | IsZero t' => IsZero (eval t')
        | _ => raise NoRule

    fun pp t = case t of
        True => print "True"
        | False => print "False"
        | If (t1, t2, t3) => (print "If(" ;
                                pp t1; print ", ";
                                pp t2; print ", ";
                                pp t3; print ")")
        | Zero => print "Zero"
        | Succ t' => (print "Succ("; pp t'; print ")")
        | Pred t' => (print "Pred("; pp t'; print ")")
        | IsZero t' => (print "IsZero("; pp t'; print ")")

    (* multi-step evaluator *)
    fun evalAll t =
        (let val t' = (eval t)
            val _ = pp t'
            val _ = print "\n"
        in evalAll t'
        end) handle NoRule => t 

    (* big-step evaluator *)
    fun evalBig t = if isValue t
                    then t
                    else case t of
            		If (t1, t2, t3) => if evalBig t1 = True
                                    then if isValue (evalBig t2)
                                        then evalBig t2
                                        else raise NoRule
                                    else if evalBig t1 = False
                                        then if isValue (evalBig t3)
                                            then evalBig t3
                                            else raise NoRule
                            			else raise NoRule
            		| Succ t1 => if isNumber (evalBig t1)
                                then Succ (evalBig t1)
                                else raise NoRule
            		| Pred t1 => if evalBig t1 = Zero
                                then Zero
                                else (case evalBig t1 of
                    				Succ t2 => if isValue t2
                                            then t2
                                            else raise NoRule
                    				| _ => evalBig t1)
            		| IsZero t1 => if evalBig t1 = Zero
                                then True
                                else (case evalBig t1 of
                    				Succ t2 => if isValue t2
                                            then False
                                            else raise NoRule
                    				| _ => evalBig t1)

end (* structure Arith *)

(* a unit test *)
fun unitTest e = (
	print "\n-----------------------\nOriginal Term:\n";
	Arith.pp e;

	print "\n\nEvaluate step by step:\n";
	Arith.evalAll e;

	print "\nEvaluate in big one:\n";
	Arith.pp (Arith.evalBig e);
	print "\n-----------------------\n"
)

(*test cases*)
val e0 = Arith.Pred (Arith.Succ (Arith.Pred Arith.Zero))
val e1 = Arith.If (Arith.True, (Arith.If (Arith.False, Arith.False, Arith.False)), Arith.True)
val e2 = Arith.Succ (Arith.Succ Arith.Zero)
val e3 = Arith.Succ Arith.True

val _ = unitTest e0
val _ = unitTest e1
val _ = unitTest e2
val _ = unitTest e3
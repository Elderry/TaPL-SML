structure Arith:ARITH = struct

    datatype t =
        True
        | False
        | If of t * t * t
        | Zero
        | Succ of t
        | Pred of t
        | IsZero of t

    fun isNumber t = case t of
        Zero => true
        | Succ t' => isNumber t'
        | _ => false

    fun isValue t = case t of
        True => true
        | False => true
        | _ =>
            if isNumber t
            then true
            else false
             
    exception NoRule
    exception Error
          
    (* one-step evaluator *)
    fun eval t = case t of
        If(True, t', _) => t' (* E-IFTRUE *)
        | If(False, _, t') => t' (* E-IFFALSE *)
        | If(t1, t2, t3) => If(eval t1, t2, t3) (* E-IF *)
        | Succ t' => Succ (eval t') (* E-SUCC *)
        | Pred Zero => Zero (* E-PREDZERO *)
        | Pred(Succ t') =>
            if isNumber t'
            then t'
            else Pred(eval(Succ t')) (* E-PREDSUCC *)
        | Pred t' => Pred(eval t') (* E-PRED *)
        | IsZero Zero => True (* E-ISZEROZERO *)
        | IsZero(Succ t') =>
            if isNumber t'
            then False
            else IsZero(eval(Succ t')) (* ISZEROSUCC *)
        | IsZero t' => IsZero (eval t') (* ISZERO *)
        | _ => raise NoRule

    fun pp t = case t of
        True => print "True"
        | False => print "False"
        | If(t1, t2, t3) => (
            print "If(";
            pp t1;
            print ", ";
            pp t2;
            print ", ";
            pp t3;
            print ")")
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
    fun evalBig t =
        if isValue t
        then t (* B-VALUE *)
        else case t of
    		If(t1, t2, t3) => (
                case evalBig t1 of
                    True =>
                        if isValue(evalBig t2)
                        then evalBig t2 (* B-IFTRUE *)
                        else raise Error
                    | False =>
                        if isValue(evalBig t3)
                        then evalBig t3 (* B-IFFALSE *)
                        else raise Error
        			| _ => raise Error)
    		| Succ t =>
                let val nv = evalBig t in
                    if isNumber(evalBig nv)
                    then Succ nv
                    else raise Error
                end (* B-SUCC *)
    		| Pred t =>
                let val t' = evalBig t in
                    if t' = Zero
                    then Zero (* B-PREDZERO *)
                    else
                        case t' of
                            Succ nv =>
                                if isNumber nv
                                then nv
                                else raise Error
                            | _ => raise Error (* B-PREDSUCC *)
                end
    		| IsZero t =>
                let val t' = evalBig t in
                    if t' = Zero
                    then True (* B-ISZEROZERO *)
                    else
                        case t' of
                            Succ nv =>
                                if isNumber nv
                                then False (* B-ISZEROSUCC *)
                                else raise Error
                            | _ => raise Error
                end
            | _ => raise Error

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
val e0 = Arith.Pred(Arith.Succ(Arith.Pred Arith.Zero))
val e1 = Arith.If(Arith.True,(Arith.If(Arith.False, Arith.False, Arith.False)), Arith.True)
val e2 = Arith.Succ(Arith.Succ Arith.Zero)

val _ = unitTest e0
val _ = unitTest e1
val _ = unitTest e2
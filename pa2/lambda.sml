structure Lambda: LAMBDA = struct

datatype t 
  = Var of string
  | Abs of string * t
  | App of t * t

exception NoRule

fun isValue t = case t of
    Abs (string, _) => true
    | _ => false

(* generate fresh variables when called *)
val counter = ref 0
fun fresh () =
    let val n = !counter
        val _ = counter := n + 1
    in concat ["x_", Int.toString n]
    end

(* in t, substitute oldName with newName *)
fun alpha (oldName:string, newName:string, t) = case t of
	Var x => if x = oldName then Var newName else Var x
	| Abs (y, t1) => if y = oldName
		then Abs (newName, alpha (oldName, newName, t1))
		else Abs (y, alpha (oldName, newName, t1))
	| App (t1, t2) => App (alpha (oldName, newName, t1), alpha (oldName, newName, t2))

(* in t12, substitute s with x *)
fun substitute (x:string, s, t12) = case t12 of
    Var y => if y = x then s else Var y
    | Abs (y, t1) => let 
    		val newName = fresh ()
    		val t2 = alpha (y, newName, t1)
    	in
    		Abs (newName, substitute(x, s, t2))
    	end
    | App (t1, t2) => App (substitute(x, s, t1), substitute(x, s, t2))

(* one-step evaluator *)
fun eval t = case t of
    Var _ => raise NoRule
    | Abs (_, _) => raise NoRule
    | App (Abs (x, t12), v2) => if isValue v2
    	then substitute (x, v2, t12)
    	else App (eval (Abs (x, t12)), v2)
    | App (v1, t2) => if isValue v1
    	then App (v1, eval t2)
    	else raise NoRule 

fun pp t =
    case t
     of Var x => print x
      | Abs (x, e) => 
        (print "\\lambda "; print x; print ".("
       ; pp e; print ")")
      | App (e1, e2) =>
        (print "("; pp e1; print ") "; print "("
       ; pp e2; print ")")

fun evalAll t =
    (let val t' = (eval t)
         val _ = pp t'
         val _ = print "\n"
     in evalAll t'
     end) handle NoRule => t 

end (* structure Lambda *)

(* a unit test *)
val omega = Lambda.Abs ("x", (Lambda.App (Lambda.Var "x", Lambda.Var "x")))

val Omega = Lambda.App (omega, omega)

val _ = (Lambda.pp Omega; print "\n")

val _ = (Lambda.pp (Lambda.eval Omega); print "\n")
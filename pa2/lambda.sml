structure Lambda: LAMBDA = struct

datatype t 
  = Var of string
  | Abs of string * t
  | App of t * t

exception NoRule

fun isValue t = case t of
    Abs (string, _) => true
    | _ => false

fun getFreeVariables t = case t of
	Var _ => [t]
	| Abs (x, t1) => 

(* generate fresh variables when called *)
val counter = ref 0
fun fresh () =
    let val n = !counter
        val _ = counter := n + 1
    in concat ["x_", Int.toString n]
    end

fun alphaConversion t =
    let fun doit (map, t) = case t of
        Var x => Var (map x)
        | Abs (x, e) => Abs (doit ( fn y => if x = y then "0" else concat ["x_", Int.toString ((map y) + 1)] , e))
        | App (e1, e2) => App(doit (map, e1), doit (map, e2))
    in  doit (fn _ => raise BadExpression, t)
    end

(* one-step evaluator *)
fun eval t = case t of
    Var _ => NoRule
    | Abs (_, _) => NoRule
    | App (Abs (x, t12), v2) => if isValue v2
        then let val x = v2 in t12 end
        else App (eval (Abs (x, t12)), v2)
    | App (v1, t2) => if isValue v1
        then App (v1, eval t2)
        else App (eval v1, t2) 

fun replace (x:string, v2, t12) case v2 of
    Var nv => case t12 of
        Var s => if s = x then Var nv else Var s
        | Abs (y, t1) => if (not y = x) andalso
    | _ => raise NoRule

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

(*val _ = Lambda.evalAll Omega*)
        

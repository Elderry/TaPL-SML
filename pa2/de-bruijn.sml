structure DeBruijn: DEBRUIJN = struct

datatype t 
  = Var of int
  | Abs of t
  | App of t * t

exception NoRule
exception BadExpression
exception Todo

fun isValue t = case t of
    Abs _ => true
    | _ => false

fun fromLambda t =
    let fun doit (map, t) = case t of
        Lambda.Var x => Var (map x)
        | Lambda.Abs (x, e) => Abs (doit ( fn y => if x = y then 0 else (map y) + 1 , e))
        | Lambda.App (e1, e2) => App(doit (map, e1), doit (map, e2))
    in  doit (fn _ => raise BadExpression, t)
    end

fun shift (d, c, t) = case t of
	Var k => if k < c then Var k else Var (k + d)
	| Abs t1 => Abs (shift (d, c + 1, t))
	| App (t1, t2) => App(shift(d, c, t1), shift(d, c, t2))

(* in t, substitute s with j *)
fun substitute (j, s, t) = case t of
    Var k => if k = j then s else Var k
    | Abs (t1) => Abs (substitute(j+1, shift(1, 0, s), t1)) 
    | App (t1, t2) => App (substitute(j, s, t1), substitute(j, s, t2))

(* we only consider closed-terms, so the 
 * complexity about naming context does not
 * concern us here.
 *)
fun eval t = case t of
    App (Abs t12, v2) => if isValue v2				
        then shift(~1, 0, substitute(0, shift(1, 0, v2), t12))	(* E-APPABS *)
        else App (Abs t12, eval v2)			                        (* E-APP2 *)
    | App (t1, t2) => App(eval t1, t2)                    			(* E-APP1 *)
    | _ => raise NoRule

fun pp t =
    case t
     of Var n => print (Int.toString n)
      | Abs t' =>
        (print "\\lambda.("; pp t'; print ")")
      | App (t1, t2) =>
        (print "("; pp t1; print ") (";
         pp t2; print ")")

fun evalAll t =
    (let val t' = (eval t)
         val _ = pp t'
         val _ = print "\n"
     in evalAll t'
     end) handle NoRule => t 

fun toLambda t = raise Todo
    
end (* end of structure DeBruijn *)

val e = Lambda.Abs ("x", Lambda.Abs ("y", Lambda.App (Lambda.Var "x", Lambda.Var "y")))

val e' = DeBruijn.fromLambda e

val _ = DeBruijn.pp e'

val id = Lambda.Abs ("x", Lambda.Var "x")

val app = Lambda.App (e, id)

val r = DeBruijn.eval (DeBruijn.fromLambda app)

val _ = print "\n\n\n"

val _ = DeBruijn.pp r
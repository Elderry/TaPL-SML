structure DeBruijn: DEBRUIJN = struct

datatype t 
  = Var of int
  | Abs of t
  | App of t * t

exception BadExpression

fun fromLambda t =
    let fun doit (map, t) = case t of
        Lambda.Var x => Var (map x)
        | Lambda.Abs (x, e) => Abs (doit ( fn y => if x = y then 0 else (map y) + 1 , e))
        | Lambda.App (e1, e2) => App(doit (map, e1), doit (map, e2))
    in  doit (fn _ => raise BadExpression, t)
    end

exception Todo
exception NoRule

(* we only consider closed-terms, so the 
 * complexity about naming context does not
 * concern us here.
 *)
fun eval t =
    raise Todo

fun evalAll t = raise Todo

fun toLambda t = raise Todo
    
fun pp t =
    case t
     of Var n => print (Int.toString n)
      | Abs t' =>
        (print "\\lambda.("; pp t'; print ")")
      | App (t1, t2) =>
        (print "("; pp t1; print ") (";
         pp t2; print ")")

end (* end of structure DeBruijn *)

val e = Lambda.Abs ("x", Lambda.Abs ("y", Lambda.App (Lambda.Var "x", Lambda.Var "y")))

val e' = DeBruijn.fromLambda e

val _ = DeBruijn.pp e'

val id = Lambda.Abs ("x", Lambda.Var "x")

val app = Lambda.App (e, id)

val r = DeBruijn.eval (DeBruijn.fromLambda app)

val _ = print "\n\n\n"

val _ = DeBruijn.pp r
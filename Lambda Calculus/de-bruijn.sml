structure DeBruijn: DEBRUIJN = struct

    datatype t 
        = Var of int
        | Abs of t
        | App of t * t

    exception NoRule
    exception BadExpression

    fun isValue t = case t of
        Abs _ => true
        | _ => false

    (* generate fresh variables when called *)
    val counter = ref 0
    fun fresh () =
        let val n = !counter
            val _ = counter := n + 1
        in concat ["x_", Int.toString n]
        end

    fun fromLambda t =
        let fun doit(map, t) = case t of
            Lambda.Var x => Var (map x)
            | Lambda.Abs(x, e) => Abs(doit(fn y => if x = y then 0 else (map y) + 1 , e))
            | Lambda.App(e1, e2) => App(doit(map, e1), doit(map, e2))
        in doit(fn _ => raise BadExpression, t)
        end

    fun shift(d, c, t) = case t of
    	Var k => if k < c then Var k else Var (k + d)
    	| Abs t1 => Abs(shift (d, c + 1, t1))
    	| App (t1, t2) => App(shift(d, c, t1), shift(d, c, t2))

    (* in t, substitute s with j, [j->s]t *)
    fun substitute(j, s, t) = case t of
        Var k => if k = j then s else Var k
        | Abs t1 => Abs(substitute(j + 1, shift(1, 0, s), t1)) 
        | App (t1, t2) => App(substitute(j, s, t1), substitute(j, s, t2))

    (* we only consider closed-terms, so the 
     * complexity about naming context does not
     * concern us here.
     *)
    fun eval t = case t of
        App(Abs t12, v2) =>
            if isValue v2				
            then shift(~1, 0, substitute(0, shift(1, 0, v2), t12)) (* E-APPABS *)
            else App (Abs t12, eval v2) (* E-APP2 *)
        | App (t1, t2) => App(eval t1, t2) (* E-APP1 *)
        | _ => raise NoRule

    fun pp t = case t of
        Var n => print (Int.toString n)
        | Abs t' => (print "\\lambda.("; pp t'; print ")")
        | App (t1, t2) => (
            print "(";
            pp t1;
            print ") (";
            pp t2;
            print ")")

    fun evalAll t = (
        let val t' = (eval t)
            val _ = pp t'
            val _ = print "\n"
        in evalAll t'
        end) handle NoRule => t 

    fun toLambda t =
        let fun doit(map, t) = case t of
            Var x => Lambda.Var (map x)
            | Abs (e) =>
            	let val newName = fresh()
                in Lambda.Abs (
    		        newName,
    		        doit(fn y => if y = 0 then newName else (map (y - 1)), e))
    		    end
            | App (e1, e2) => Lambda.App(doit (map, e1), doit (map, e2))
        in doit(fn _ => raise BadExpression, t)
        end
    
end (* end of structure DeBruijn *)

val e = Lambda.Abs ("x", Lambda.Abs ("y", Lambda.App (Lambda.Var "x", Lambda.Var "y")))

val e' = DeBruijn.fromLambda e

val e'' = DeBruijn.toLambda e'

val _ = DeBruijn.pp e'
val _ = print "\n\n\n"

val id = Lambda.Abs("x", Lambda.Var "x")

val _ = Lambda.pp id
val _ = print "\n\n\n"

val app = Lambda.App(e, id)

val _ = Lambda.pp app
val _ = print "\n\n\n"

val _ = Lambda.pp e''
val _ = print "\n\n\n"

val r = DeBruijn.eval(DeBruijn.fromLambda app)

val _ = DeBruijn.pp r
val _ = print "\n\n\n"
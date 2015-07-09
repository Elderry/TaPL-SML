structure SystemF:SYSTEMF = struct

    structure Type = struct

        datatype t =
            Bool
            | Var of string
            | Arrow of t * t
            | Product of t * t
            | Forall of string * t
            | Exists of string * t

        fun equals (t1, t2) = case (t1, t2) of
            (Bool, Bool) => true
            | (Var x, Var y) => x=y
            | (Arrow (a1, b1), Arrow (a2, b2)) => equals (a1, a2) andalso equals (b1, b2)
            | (Product (t1, t2), Product (s1, s2)) => equals (t1, s1) andalso equals (t2, s2)
            | (Forall (x, t1), Forall (y, t2)) => x=y andalso equals(t1, t2)
            | (Exists (x, t1), Exists (y, t2)) => x=y andalso equals (t1, t2)
            | _ => false
            
        fun pp t = case t of
            Bool => print "Bool"
            | Var x => print x
            | Arrow (t1, t2) => (
                print "(";
                pp t1;
                print "->";
                pp t2;
                print ")")
            | Product (t1, t2) => (
                print "(";
                pp t1;
                print " * ";
                pp t2;
                print ")")      
            | Forall (x, t) => (
                print "(\\forall ";
                print x;
                print ".";
                pp t;
                print ")")
            | Exists (x, t) => (
                print "(\\exists ";
                print x;
                print ".";
                pp t;
                print ")")

        val counter = ref 0
        fun fresh () =
            let val n = !counter
                val _ = counter := (n+1)
            in  String.concat ["X_", Int.toString n]
            end
        
        fun alpha (t) =
            let fun doit (G, t) = case t of
                Bool => Bool
                | Var x => Var (G x)
                | Arrow(t1, t2) => Arrow (doit (G, t1), doit(G, t2))
                | Product(t1, t2) => Product (doit (G, t1), doit (G, t2))
                | Forall(x, t) =>
                    let val new = fresh ()
                    in Forall(new, doit(fn z => if z=x then new else G z, t))
                    end
                | Exists (x, t) =>
                    let val new = fresh ()
                    in Exists (new, doit(fn z => if z=x then new else G z, t))
                    end
            in doit(fn x => x, t)
            end
        
        (* [x|->t1]t2 *)
        fun subst (x, t1, t2) = case t2 of
            Bool => Bool
            | Var y => if x=y then t1 else t2
            | Arrow (s1, s2) => Arrow (subst (x, t1, s1), subst(x, t1, s2))
            | Product (s1, s2) => Product (subst (x, t1, s1), subst (x, t1, s2))
            | Forall (x, t') =>
                let val (y, t'') = case alpha (t2) of
                    Forall (y, t'') => (y, t'')
                    | _ => raise Fail "bug"
                in Forall (y, subst (x, t1, t''))
                end
            | Exists (x, t') =>
                let val (y, t'') = case alpha (t2) of
                    Exists (y, t'') => (y, t'')
                    | _ => raise Fail "bug"
            in Exists(y, subst (x, t1, t''))
            end

        fun isFree (x, t) = case t of
            Bool => false
            | Var y => if x=y then true else false
            | Arrow (t1, t2) => isFree (x, t1) orelse isFree (x, t2)
            | Product (t1, t2) => isFree (x, t1) orelse isFree (x, t2)
            | Forall (y, t') =>
                let val (z, t'') = case alpha (t) of
                    Forall (z, t'') => (z, t'')
                    | _ => raise Fail "bug"
                in isFree (x, t'')
                end
            | Exists (y, t') =>
                let val (z, t'') = case alpha (t) of
                    Exists (z, t'') => (z, t'')
                    | _ => raise Fail "bug"
                in isFree (x, t'')
                end
                 
    end (* structure Type *)

    datatype t =
    True
    | False
    | If of t * t * t
    | Var of string
    | Abs of string * Type.t * t
    | App of t * t
    | Pair of t * t
    | First of t
    | Second of t
    | TyAbs of string * t
    | TyApp of t * Type.t
    | Pack of Type.t * t * Type.t
    | Unpack of string * string * t * t
                
    exception TypeError of string

    fun check(G:string -> Type.t, D:string -> bool, t):Type.t = raise Todo
                   

    fun typeCheck t = 
        let val ty =
            check(
                fn x => raise TypeError "var not found",
                fn x => raise TypeError "type var not found",
                t)
        in ty
        end

    fun pp t = case t of
        True => print "true"
        | False => print "false"
        | If (e1, e2, e3) => (
            print "if ";
            pp e1;
            print " then ";
            pp e2;
            print " else ";
            pp e3)
        | Var x => print x
        | Abs(x, ty, t) => (
            print "\\lambda ";
            print x;
            print ":";
            Type.pp ty;
            print ".";
            pp t)
        | App (t1, t2) => (
            print "(";
            pp t1;
            print ") (";
            pp t2;
            print ")")
        | Pair (t1, t2) => (
            print "(";
            pp t1;
            print ", ";
            pp t2;
            print ")")
        | First t => (
            print "#1(";
            pp t;
            print ")")
        | Second t => (
            print "#2(";
            pp t;
            print ")")
        | TyAbs (x, t) => (
            print "\\Lambda ";
            print x;
            print ".";
            pp t)
        | TyApp (t, ty) => (
            print "(";
            pp t;
            print ")";
            print "[";
            Type.pp ty;
            print "]")
        | Pack(ty, t, ty') => (
            print "pack {";
            Type.pp ty;
            print ", ";
            pp t;
            print "} as ";
            Type.pp ty')
        | Unpack(ty, x, t1, t2) => (
            print "unpack {";
            print ty;
            print ", ";
            print "x} = ";
            pp t1;
            print " in ";
            pp t2) 

(* unit test *)
        
    (* (\Lambda X.\lambda x:X.x) [Bool] true *)
    val t11 = TyAbs ("X", Abs ("x", Type.Var "X", Var "x"))
    val t1 = App(TyApp(t11, Type.Arrow(Type.Bool, Type.Bool)), TyApp(t11, Type.Bool))
    val _ = (pp t1; print "\n")
    val ty1 = typeCheck t1
    val _ = (Type.pp ty1; print "\n")

(* pack {Bool, (true, \lambda x:Bool.x)} as Exists{X, 
 *      Product (X, X->X)}
 *)

    val t2 =
        Pack(
            Type.Bool,
            Pair(True, Abs("x", Type.Bool, Var "x")),
            Type.Exists(
                "X",
                Type.Product(Type.Var "X", Type.Arrow(Type.Var "X", Type.Var "X"))))

val ty2 = typeCheck t2
val _ = (Type.pp ty2; print "\n")
        
(* Type error! *)
val t3 = Unpack ("X", "x", t2, App(Second(Var "x"), First (Var "x")))
val ty3 = typeCheck t3
val _ = (Type.pp ty3; print "\n")

end
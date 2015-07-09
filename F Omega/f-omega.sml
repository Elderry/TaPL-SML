structure FOmega:F_OMEGA = struct

    structure Kind = struct

        datatype t =
            Star
            | KArrow of t * t

        fun pp t = case t of
            Star => print "*"
            | KArrow (t1, t2) => (
                print "(";
                pp t1;
                print "=>";
                pp t2;
                print ")")

        fun equals (k1, k2) = case (k1, k2) of
            (Star, Star) => true
            | (KArrow (s1, s2), KArrow (s3, s4)) => equals(s1, s3) andalso equals(s2, s4)
            | _ => false
        
    end (* structure Kind *)

    structure Con = struct

        datatype t = Bool
            | Var of string
            | Arrow of t * t
            | Forall of string * Kind.t * t
            | TyAbs of string * Kind.t * t
            | TyApp of t * t
             
        fun pp t = case t of
            Bool => print "Bool"
            | Var x => print x
            | Arrow (t1, t2) => (
                print "(";
                pp t1;
                print "->";
                pp t2;
                print ")")        
            | Forall (X, k, t) => (
                print "\\forall ";
                print X;
                print "::";
                Kind.pp k;
                print ".";
                pp t)
            | TyAbs (X, k, t) => (
                print "\\Lambda ";
                print X;
                print "::";
                Kind.pp k;
                print ".";
                pp t)
            | TyApp (t1, t2) => (
                print "(";
                pp t1;
                print ") (";
                pp t2;
                print ")")

        val counter = ref 0
        fun fresh () =
            let val n = !counter
                val _ = counter := (n+1)
            in  String.concat ["X_", Int.toString n]
            end
        
        fun alpha t =
            let fun doit(G, t) = case t of
                Bool => Bool
                | Var x => Var (G x)
                | Arrow(t1, t2) => Arrow(doit(G, t1), doit(G, t2))
                | Forall(X, K, t) =>
                    let val new = fresh ()
                    in Forall(
                        new,
                        K,
                        doit(fn z => if z = X then new else G z, t))
                    end
                | TyAbs(X, K, t) =>
                    let val new = fresh ()
                    in TyAbs(
                        new,
                        K,
                        doit(fn z => if z = X then new else G z, t))
                    end
                | TyApp(t1, t2) => TyApp(doit(G, t1), doit(G, t2))
            in doit(fn x => x, t)
            end
        
        (* [x|->t1]t2 *)
        fun subst(x, t1, t2) = case t2 of
            Bool => Bool
            | Var y => if x=y then t1 else t2
            | Arrow (s1, s2) => Arrow(subst(x, t1, s1), subst(x, t1, s2))
            | Forall (X, K, t') =>
                let val (Y, t2') = case alpha (t2) of
                    Forall(Y, _, t'') => (Y, t'')
                    | _ => raise Fail "bug"
                in Forall(Y, K, subst(x, t1, t2'))
                end
            | TyAbs (X, K, t') =>
                let val (Y, t2') = case alpha (t2) of
                    TyAbs(Y, _, t'') => (Y, t'')
                    | _ => raise Fail "bug"
                in TyAbs(Y, K, subst(x, t1, t2'))
                end
            | TyApp (s1, s2) => TyApp(subst(x, t1, s1), subst(x, t1, s2))

    end (* structure Con *)

    datatype t =
        True
        | False
        | If of t * t * t
        | Var of string
        | Abs of string * Con.t * t
        | App of t * t
        | TyAbs of string * Kind.t * t
        | TyApp of t * Con.t
 
    exception TypeError of string

    (* structural equivalence *)
    fun strEq(D: string -> Kind.t, c1, c2):bool = case (c1, c2) of
        (Con.Bool, Con.Bool) => true
        | (Con.Arrow(c11, c12), Con.Arrow(c21, c22)) =>
            if algEq(D, c11, c21, Kind.Star) andalso algEq(D, c12, c22, Kind.Star)
            then true
            else false
        | (Con.Forall(a, K1, c1), Con.Forall(b, K2, c2)) =>
            a = b
            andalso Kind.equals(K1, K2)
            andalso algEq(D, c1, c2, Kind.Star)
        | (Con.Var X, Con.Var Y) => if X = Y then true else false
        | (Con.TyApp(Con.Var X, c1'), Con.TyApp(Con.Var Y, c2')) => (
            case kindCheck(D, Con.Var X) of
                Kind.KArrow(K, Kind.Star) =>
                    if algEq(D, c1', c2', K)
                    then true
                    else false
                | _ => false)
        | _ => false
          
    (* algorithmic equivalence*)
    and algEq(D: string -> Kind.t, c1, c2, k):bool = case k of
        Kind.Star =>
            let val c1' = evalCon(D, c1)
                val c2' = evalCon(D, c2)
            in strEq(D, c1', c2')
            end
        | Kind.KArrow(K1, K2) =>
            algEq(
                fn x => if x = "a" then K1 else D x,
                Con.TyApp(c1, Con.Var "a"),
                Con.TyApp(c2, Con.Var "a"),
                K2)
          
    (* \beta-normalization *)
    and evalCon(D: string -> Kind.t, c) = case c of
        Con.TyApp (c1, c2) =>
            let val c1' = evalCon (D, c1)
            in case c1' of
                Con.TyAbs(X, k, cbody) => evalCon(D, Con.subst(X, c2, cbody))
                | _ => Con.TyApp(c1', c2)
            end
        | _ => c

    (* kind checking *)
    and kindCheck(D: string -> Kind.t, c):Kind.t = case c of
        Con.Bool => Kind.Star
        | Con.Var X => D X
        | Con.TyAbs(X, K, T) =>
            kindCheck(
                fn L => if L = X then K else D L,
                T)
        | Con.TyApp(T1, T2) =>
            let val K1 = kindCheck(D, T1)
                val K2 = kindCheck(D, T2)
            in case K1 of
                Kind.KArrow(K11, K12) =>
                    if Kind.equals(K2, K11)
                    then K12
                    else raise TypeError ""
                | _ => raise TypeError ""
            end
        | Con.Arrow(T1, T2) =>
            if Kind.equals(kindCheck(D, T1), Kind.Star)
                andalso Kind.equals(kindCheck(D, T2), Kind.Star)
            then Kind.Star
            else raise TypeError ""
        | Con.Forall(a, K, c) =>
            if Kind.equals(
                kindCheck(
                    fn a => if a = "a" then K else D a,
                    c),
                Kind.Star)
            then Kind.Star
            else raise TypeError ""
                  
    (* type checking *)
    fun check(G: string -> Con.t, D: string -> Kind.t, t):Con.t = case t of
        True => Con.Bool
        | False => Con.Bool
        | If(t1, t2, t3) => (case evalCon(D, check(G, D, t1)) of
            Con.Bool =>
                let val c1 = check(G, D, t2)
                    val c2 = check(G, D, t3)
                in if algEq(D, c1, c2, Kind.Star)
                    then c1
                    else raise TypeError ""
                end
            | _ => raise TypeError "") (* T-IF *)
        | Var x => G x
        | Abs(x, c, t) =>
            if Kind.equals(kindCheck(D, c), Kind.Star)
            then
                Con.Arrow(
                    c,
                    check(fn y => if y = x then c else G y, D, t))
            else raise TypeError ""
        | App(t1, t2) => (case evalCon(D, check(G, D, t1)) of
            Con.Arrow(c2, c3) =>
                let val c4 = check(G, D, t2)
                in if algEq(D, c2, c4, Kind.Star)
                    then c3
                    else raise TypeError ""
                end
            | _ => raise TypeError "")
        | TyAbs(a, K, t) =>
            let val c =
                check(
                    G,
                    fn b => if b = a then K else D b,
                    t)
            in Con.Forall(a, K, c)
            end
        | TyApp(t, c) => case check(G, D, t) of
            Con.Forall(a, K, c') =>
                if Kind.equals(K, kindCheck(D, c))
                then Con.subst(a, c, c')
                else raise TypeError ""
            | _ => raise TypeError ""

    fun typeCheck t = 
        check(
            fn x => raise TypeError "var not found",
            fn x => raise TypeError "type var not found",
            t)

    fun pp t = case t of
        True => print "true"
        | False => print "false"
        | If(e1, e2, e3) => (
            print "if ";
            pp e1;
            print " then ";
            pp e2;
            print " else ";
            pp e3)
        | Var x => print x
        | Abs (x, c, t) => (
            print "\\lambda ";
            print x;
            print ":";
            Con.pp c;
            print ".";
            pp t)
        | App(t1, t2) => (
            print "(";
            pp t1;
            print ") (";
            pp t2;
            print ")")
        | TyAbs (x, k, t) => (
            print "\\tlambda ";
            print x;
            print "::";
            Kind.pp k;
            print ".";
            pp t)
        | TyApp (t, c) => (
            print "(";
            pp t;
            print ") (";
            Con.pp c ;
            print ")")

    (* unit test *)
    val println = fn s => (print s; print "\n")
    val printb = fn b => (println (Bool.toString b))
        
    (* D |- \Lambda X::*.X <=> \Lambda Y::*.Y *)
    val b1 = algEq(
        fn x => raise Fail "bug",
        Con.TyAbs ("X", Kind.Star, Con.Var "X"),
        Con.TyAbs ("Y", Kind.Star, Con.Var "Y"),
        Kind.KArrow (Kind.Star, Kind.Star))
    val _ = printb b1

        
    (* D |- Bool <=> (\Lambda X::*X) Bool *)
    val b2 = algEq(
        fn x => raise Fail "bug",
        Con.Bool,
        Con.TyApp(Con.TyAbs("X", Kind.Star, Con.Var "X"), Con.Bool),
        Kind.Star)
    val _ = printb b2

    (* D |- \Lambda f::*=>*.f <=> \Lambda g::*=>*.\Lambda X::*.g X *)
    val b3 = algEq(
        fn x => raise Fail "bug",
        Con.TyAbs("f", Kind.KArrow(Kind.Star, Kind.Star), Con.Var "f"),
        Con.TyAbs (
            "g",
            Kind.KArrow(Kind.Star, Kind.Star),
            Con.TyAbs(
                "X",
                Kind.Star,
                Con.TyApp(Con.Var "g", Con.Var "X"))),
        Kind.KArrow(
            Kind.KArrow(Kind.Star, Kind.Star),
            Kind.KArrow(Kind.Star, Kind.Star)))
    val _ = printb b3

    (* D |- \Lambda f::*=>*.\Lambda g::*=>*.\Lambda X::*. f X
     *    <=> \Lambda f::*=>*.\Lambda g::*=>*.\Lambda X::*. g X *)
    val karrow = Kind.KArrow (Kind.Star, Kind.Star)
    val doublearrow = Kind.KArrow (karrow, karrow)
                  
    val b4 = algEq(
        fn x => raise Fail "bug",
        Con.TyAbs(
            "f",
            Kind.KArrow(Kind.Star, Kind.Star),
            Con.TyAbs(
                "g",
                Kind.KArrow(Kind.Star, Kind.Star),
                Con.TyAbs(
                    "X",
                    Kind.Star,
                    Con.TyApp(Con.Var "f", Con.Var "X")))),
        Con.TyAbs(
            "f",
            Kind.KArrow(Kind.Star, Kind.Star),
            Con.TyAbs (
                "g",
                Kind.KArrow(Kind.Star, Kind.Star),
                Con.TyAbs(
                    "X",
                    Kind.Star,
                    Con.TyApp(Con.Var "g", Con.Var "X")))),
        Kind.KArrow(doublearrow, karrow))
    val _ = printb b4

    (* D |- \Lambda c::*. (\Lambda X::*.\Lambda c::*.X) c
     *    <=> \Lambda c::*.\Lambda c::*.c *)
    val b5 = algEq(
        fn x => raise Fail "bug",
        Con.TyAbs(
            "c",
            Kind.Star,
            Con.TyApp(
                Con.TyAbs(
                    "X",
                    Kind.Star,
                    Con.TyAbs("c", Kind.Star, Con.Var "X")),
                Con.Var "c")),
        Con.TyAbs(
            "c",
            Kind.Star,
            Con.TyAbs(
                "c",
                Kind.Star,
                Con.Var "c")),
        Kind.KArrow(Kind.Star, karrow))
    val _ = printb b5

end
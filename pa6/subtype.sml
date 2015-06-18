structure Subtype:SUBTYPE = struct

  structure Type = struct
    datatype t =
        Record of (string*t) list
        | Top
        | String
        | Arrow of t * t
             
    end (* structure Type *)

    datatype t =
        Record of (string * t) list
        | Proj of t * string
        | String of string
        | Var of string
        | Abs of string * Type.t * t
        | App of t * t

    exception Todo
    exception TypeError of string

    fun check (env, t) = raise Todo

    fun typeCheck t = check (fn x => raise TypeError "var not found", t)

    fun pp t = case t of
        Record l =>
            let fun ppfields l =
                case l of
                    [] => ()
                    | (name, t)::xs => (
                        print name;
                        print " = ";
                        pp t;
                        print "; ";
                        ppfields xs)
            in (
                print "{";
                ppfields l;
                print "}")
            end
        | Proj (t, name) => (
            pp t;
            print ".";
            print name)
        | String s => print s
        | Var x => print x
        | Abs (x, ty, t) => (
            print "\\lambda ";
            print x;
            print ": ";
            Type.pp ty;
            print ".";
            pp t)
        | App (t1, t2) => (
            print "(";
            pp t1;
            print ") (";
            pp t2;
            print ")")

    (* unit test *)
    val t = App(
        Abs("x",
            Type.Record [("name" , Type.String)],
            Proj(Var "x" , "name")),
        Record [("age", String "20"),
        ("name", String "Bob")])
    val _ = (pp t; print "\n")
    val ty = typeCheck t

end
structure Let:LET = struct

	structure Type = struct

		datatype t =
			Bool
	  		| Var of string
	  		| Arrow of t * t
	  		| Pair of t * t

		datatype scheme = Forall of string list * t

		fun map(f, t) = case t of
			Bool => Bool
      		| Var x => f x
      		| Arrow(t1, t2) => Arrow(map(f, t1), map(f, t2))
      		| Pair(t1, t2) => Pair(map(f, t1), map(f, t2))

		fun exists (x, l) = case l of
			[] => false
      		| y::ys => if x=y then true else exists(x, ys)

		fun fvs t =
		    let fun doit(t, l) = case t of
		    	Bool => l
              	| Var x => if exists(x, l) then l else x::l
              	| Arrow (t1, t2) => doit(t2, doit (t1, l))
              	| Pair (t1, t2) => doit(t2, doit (t1, l))
		    in doit(t, [])
		    end

		fun generalize (t): scheme =
		    let val vars = fvs t
		    in Forall(vars, t)
		    end
      
		fun pp t = case t of
			Bool => print "Bool"
			| Var x => print x
      		| Arrow (t1, t2) => (
      			print "(";
      			pp t1;
      			print "->";
      			pp t2;
      			print ")")
      		| Pair (t1, t2) => (
      			print "(";
      			pp t1;
      			print "*";
      			pp t2;
      			print ")")
        
		fun occurs(x, t) = case t of
			Bool => false
      		| Var y => if x=y then true else false
      		| Arrow(t1, t2) => occurs(x, t1) orelse occurs(x, t2)
      		| Pair(t1, t2) => occurs(x, t1) orelse occurs(x, t2) 

		(* [x|->t1]t2 *)
		fun subst (x, t1, t2) = case t2 of
			Bool => Bool
      		| Var y => if x=y then t1 else t2
      		| Arrow(s1, s2) => Arrow(subst(x, t1, s1), subst(x, t1, s2))
      		| Pair(s1, s2) => Pair(subst(x, t1, s1), subst(x, t1, s2))
             
	end (* structure Type *)

	structure Constraint = struct

		type t = (Type.t * Type.t) list

		fun empty (): t = []

		fun subst (x, ty, t) =
		    List.map(
		    	fn (s1, s2) => (
		    		Type.subst(x, ty, s1),
		    		Type.subst (x, ty, s2)))
		    t

		fun solve(t) = raise Todo

		fun pp t =
			List.app(
				fn(x, y) => (
					Type.pp x;
					print "=";
					Type.pp y;
					print "; "))
			t

	end (* strucure Constraint *)

	datatype t =
		True
	  	| False
	  	| If of t * t * t
	  	| Var of string
	  	| Abs of string * t
	  	| App of t * t
  		| Let of string * t * t
  		| Pair of t * t

	exception TypeError of string

	val counter = ref 0
	fun freshTypeVar() =
	    let val n = !counter
	        val _ = counter := (n+1)
	    in  String.concat ["X_", Int.toString n]
	    end

	fun instantiate (Type.Forall (vars, t)) =
	    let fun doit(vars, t) = case vars of
	    	[] => t
            | x::xs =>
                let val new = freshTypeVar ()
                in doit(xs, Type.subst(x, Type.Var new, t))
                end
	    in doit(vars, t)
	    end
    
	datatype opt =
		Left of Type.t
  		| Right of Type.scheme
        
	fun check(env: string -> opt, t): Type.t * Constraint.t = 
    
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
    	| Abs (x, t) => (
    		print "\\lambda ";
    		print x;
    		print ".";
    		pp t)
      	| App (t1, t2) => (
      		print "(";
      		pp t1;
      		print ") (";
      		pp t2;
      		print ")")
      	| Let (x, t1, t2) => (
      		print "let ";
      		print x;
      		print "=";
      		pp t1;
      		print " in ";
      		pp t2;
      		print " end")
      	| Pair(t1, t2) => (
      		print "(";
      		pp t1;
      		print ", ";
      		pp t2;
      		print ")")

	fun typeCheck t = 
	    let val (ty, c) =
		    	check(
		    		fn x =>
			    		let val _ = print x
			    		in raise TypeError "var not found"
			    		end,
			    	t)
        	val _ = print "\nthe term: "
        	val _ = pp t
        	val _ = print "\nConstraints: "
        	val _ = Constraint.pp c
        	val _ = print "\nraw type: "
        	val _ = Type.pp ty
        	val map = Constraint.solve (c)
        	val ty = Type.map (map, ty)
        	val _ = print "\npretty type: "
        	val _ = Type.pp ty
	    in  ty
	    end

	(* unit test *)
	val t = App(Abs("x", Var "x"), True)
	val ty = typeCheck t

	val t1 = Let ("f", Abs("x", Var "x"), App (Var "f", Var "f"))
	val ty1 = typeCheck t1

	val t2 = Let(
		"f0",
		Abs("x0", Pair (Var "x0", Var "x0")),
		Let(
			"f1",
			Abs(
				"x1",
				App(Var "f0", App(Var "f0", Var "x1"))),
			App(Var "f1", Abs ("z", Var "z"))))
	val ty2 = typeCheck t2
	(*
	val t2 = Abs ("f", App (Var "f", Var "f"))
	val ty2 = typeCheck t2 *)

end
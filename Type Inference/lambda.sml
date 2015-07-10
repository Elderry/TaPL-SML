structure Lambda:LAMBDA = struct

	structure Type = struct

		datatype t =
			Bool
  			| Var of string
  			| Arrow of t * t

  		fun equals(t1, t2) = case (t1, t2) of
  			(Bool, Bool) => true
  			| (Var X, Var Y) => X = Y
  			| (Arrow(t11, t12), Arrow(t21, t22)) => equals(t11, t21) andalso equals(t12, t22)
  			| _ => false

  		fun isFreeIn(X, T) = case T of
  			Bool => false
  			| Var Y => X = Y
  			| Arrow(T1, T2) => isFreeIn(X, T1) orelse isFreeIn(X, T2)

		fun map(f, t) = case t of
			Bool => Bool
      		| Var x => f x
      		| Arrow(t1, t2) => Arrow(map(f, t1), map(f, t2))

		fun pp t = case t of
			Bool => print "Bool"
      		| Var x => print x
      		| Arrow (t1, t2) => (
      			print "(";
      			pp t1;
      			print "->";
      			pp t2;
      			print ")")

		(* [x|->t1]t2 *)
		fun subst(x, t1, t2) = case t2 of
			Bool => Bool
      		| Var y => if x=y then t1 else t2
      		| Arrow(s1, s2) => Arrow(subst(x, t1, s1), subst(x, t1, s2))
             
	end (* structure Type *)

	structure Constraint = struct

		type t = (Type.t * Type.t) list

		exception ConstraintError

		fun empty():t = []

		fun subst(X, T, C) = case C of
			[] => []
			| (L, R)::C' => [(Type.subst(X, T, L), Type.subst(X, T, R))] @ (subst(X, T, C'))

		fun solve C = case C of
			[] => (fn X => raise ConstraintError)
			| (S, T)::C' =>
				if Type.equals(S, T)
				then solve C'
				else case (S, T) of
					(Type.Var X, _) =>
						if not (Type.isFreeIn(X, T))
						then fn Y => if Y = X then T else solve(subst(X, T, C')) Y
						else raise ConstraintError
					| (_, Type.Var X) =>
						if not (Type.isFreeIn(X, S))
						then fn Y => if Y = X then S else solve(subst(X, S, C')) Y
						else raise ConstraintError
					| (Type.Arrow(S1, S2), Type.Arrow(T1, T2)) =>
						solve(C' @ [(S1, T1), (S2, T2)])
					| _ => raise ConstraintError

		fun pp t =
			List.app(
				fn(x, y) => (
					Type.pp x;
					print "=";
					Type.pp y;
					print "; "))
			t
    
	end (* strucure Constraint *)

	datatype t
  		= True
  		| False
  		| If of t * t * t
  		| Var of string
  		| Abs of string * t
  		| App of t * t

	exception TypeError of string

	val counter = ref 0
	fun freshTypeVar() =
	    let val n = !counter
	        val _ = counter := (n+1)
	    in  String.concat ["X_", Int.toString n]
	    end

	fun check(env: string -> Type.t, t): Type.t * Constraint.t = case t of
		True => (Type.Bool, [])
		| False => (Type.Bool, [])
		| If(t1, t2, t3) =>
			let val (T1, C1) = check(env, t1)
				val (T2, C2) = check(env, t2)
				val (T3, C3) = check(env, t3)
			in (T2, C1 @ C2 @ C3 @ [(T1, Type.Bool), (T2, T3)])
			end
		| Var x => (env x, [])
		| Abs(x, t') =>
			let val X = Type.Var (freshTypeVar())
				val (T, C) = check(fn y => if y = x then X else env y, t')
			in (Type.Arrow(X, T), C)
			end
		| App(t1, t2) =>
			let val (T1, C1) = check(env, t1)
				val (T2, C2) = check(env, t2)
				val X = Type.Var (freshTypeVar())
			in (X, C1 @ C2 @ [(T1, Type.Arrow(T2, X))])
			end

	fun typeCheck t = 
	    let val (ty, c) = check(fn x => raise TypeError "var not found", t)
	        val _ = Constraint.pp c
	        val _ = print "\n"
	        val _ = Type.pp ty
	        val _ = print "\n"
	        val map = Constraint.solve c
	        val ty = Type.map (map, ty)
	        val _ = Type.pp ty
	        val _ = print "\n"
	    in ty
	    end

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

	(* unit test *)
	val t = App(Abs("x", Var "x"), True)
	val _ = (pp t; print "\n")
	val ty = typeCheck t

end
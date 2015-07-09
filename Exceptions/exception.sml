structure Exception: EXCEPTION = struct

	structure Type = struct
		exception TypeError
		datatype t =
			Any
		    | Int
    
		fun equals(t1, t2) = true
		fun toString t = case t of
			Any => "Any"
			| Int => "Int"
	end

	datatype t =
	    Num of int
	    | Add of t * t
	    | Throw
	    | Try of t * t

	datatype Frame =
		LeftAdd of t (* [] + e *)
		| RightAdd of t (* v + [] *)
		| FTry of t (* try [] catch e *)

	type Stack = Frame list

	fun typeCheck t = case t of
		Num _ => Type.Int
		| Add (t1, t2) =>
			let val ty1 = typeCheck t1
				val ty2 = typeCheck t2
			in
				if
					Type.equals(ty1, Type.Int)
					andalso Type.equals(ty2, Type.Int)
				then Type.Int
				else Type.Any
			end
		| Throw => Type.Any
		| Try (t1, t2) =>
			let val ty1 = typeCheck t1
				val ty2 = typeCheck t2
			in
				if Type.equals(ty1, ty2)
				then ty1
				else raise Type.TypeError
			end

	fun unwinding(stack:Stack):Stack = case stack of
		[] => []
		| F::S => (
			case F of
				FTry _ => stack
				| _ => unwinding S)

	exception AppError
	fun app(frame:Frame, t:t):t =
		case t of
			Num n => (
				case frame of
					LeftAdd e => Add(t, e)
					| RightAdd v => (
						case v of
							Num n' => Num(n + n')
							| _ => raise AppError)
					| FTry e => t)
			| _ => raise AppError
        
	exception NoRule of t
 
	fun eval(stack:Stack, t:t) = case t of
		Num n => (
			case stack of
				F::S => eval(S, app(F, (Num n))) (* E-Value *)
				| _ => raise NoRule t)
		| Add(Num n1, Num n2) => eval(stack, Num(n1 + n2)) (* E-Add1 *)
		| Add(Num n, t) => eval(RightAdd(Num n)::stack, t) (* E-Add2 *)
		| Add(t1, t2) => eval(LeftAdd t2::stack, t1) (* E-Add3 *)
		| Try(Num n, e2) => eval(stack, Num n) (* E-TryV *)
		| Try(e1, e2) => eval(FTry e2::stack, e1)
		| Throw =>
			let val S = unwinding stack
			in (case S of
				FTry e2::S2 => eval(S2, e2)
				| _ => raise NoRule t)
			end

	fun pp t = case t of
        Num n => print (Int.toString n)
        | Add (t1, t2) => (pp t1; print " + "; pp t2)
        | Throw => print "throw"
        | Try (t1, t2) => (
        	print "(try ";
        	pp t1;
        	print " catch ";
        	pp t2;
        	print ")")

	fun evalAll' stack t = (
		let val (stack', t') = eval(stack, t)
            val _ = pp t'
            val _ = print "\n"
        in  evalAll' stack' t'
        end) handle NoRule t => t 

	val evalAll = evalAll' []

	(* unit test *)
	val t = Num 8
	val _ = (print "case 1: "; pp t; print "\n")
	val ty = typeCheck t
	val t' = evalAll t
	val _ = (print "Type: "; print (Type.toString(ty)); print "\n")
	val _ = (print "After evaluation: "; pp t'; print "\n\n")

	val t = Try (Try (Throw, Num 3), Num 5)
	val _ = (print "case 2: "; pp t; print "\n")
	val ty = typeCheck t
	val t' = evalAll t
	val _ = (print "Type: "; print (Type.toString(ty)); print "\n")
	val _ = (print "After evaluation: "; pp t'; print "\n\n")

	val t = Throw
	val _ = (print "case 3: "; pp t; print "\n")
	val ty = typeCheck t
	val t' = evalAll t
	val _ = (print "Type: "; print (Type.toString(ty)); print "\n")
	val _ = (print "After evaluation: "; pp t'; print "\n\n")

end
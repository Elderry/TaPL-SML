structure Exception: EXCEPTION = struct

	structure Type = struct
		datatype t =
			Any
		    | Int
    
		fun equals(t1, t2) = true
	end

	datatype t =
    Num of int
    | Add of t * t
    | Throw
    | Try of t * t

	exception Todo

	fun typeCheck t = raise Todo

	fun unwinding stack a = raise Todo
        
	exception NoRule of t
 
	fun eval (stack, t) = raise Todo

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
		let val (stack', t') = eval (stack, t)
            val _ = pp t'
            val _ = print "\n"
        in  evalAll' stack' t'
        end) handle NoRule t => t 

	val evalAll = evalAll' []

	(* unit test *)
	val t = Num 8
	val _ = (pp t; print "\n")
	val _ = typeCheck t
	val _ = evalAll t

	val t = Try (Try (Throw, Num 3), Num 5)
	val _ = (pp t; print "\n")
	val _ = typeCheck t
	val _ = evalAll t

	val t = Throw
	val _ = (pp t; print "\n")
	val _ = typeCheck t
	val _ = evalAll t

end
structure Reference: REFERENCE = struct

	structure Type = struct

		datatype t =
			Bool
			| Unit
  			| Fun of t * t
  			| Ref of t
           
		fun equals (t1, t2) = case (t1, t2) of
		    (Bool, Bool) => true
	      	| (Fun (s1, s2), Fun (s3, s4)) => equals (s1, s3)
	      									andalso equals (s2, s4)
	        | (Ref s1, Ref s2) => equals (s1, s2)
	        | (Unit, Unit) => true
	        | _ => false
             
		fun toString t = case t of
			Bool => "bool"
		    | Unit => "unit"
		    | Fun (t1, t2) => String.concat [toString t1, " -> ", toString t2]
		    | Ref t => String.concat [toString t, " ref"]

		exception TypeError

	end (* structure Type *)

(* we extend the language of lambda-calculus
 * with reference, as we discussed in class: there
 * are 3 reference-related operations:
 *   1. create a fresh reference with initial value e: ref e
 *   2. read the value pointed by a reference e: !e, and
 *   3. update the address pointed by e1 with the value of e2: e1 := e2.
 *)
	datatype t =
    	True
    	| False
  		| If of t * t * t
  		| Var of string
  		| Abs of string * Type.t * t
  		| App of t * t
  		| Ref of t          (* ref e *)
  		| Deref of t        (* !e *)
  		| Assign of t * t   (* e1 := e2 *)
  		| Address of string (* l *)
  		| Unit
        
	fun typeCheck (t: t): Type.t = check(
		fn x => Type.TypeError,
		fn x => BadAddress,
		t)

	fun check (context, storeTyping, t) = case t of
		True => Type.Bool
		| False => Type.Bool
		| Var x => context x (* T-VAR *)
		| Abs (x, ty, t2) =>
			check(
				fn y => if y = x then ty else context y,
				storeTyping,
				t2) (* T-ABS *)
		| App (t1, t2) =>
			let val ty2 = check (context, storeTyping, t2)
			in
				case check (context, storeTyping, t1) of
					Type.Fun (ty1, ty3) =>
						if (ty1 = ty2)
						then ty3
						else raise Type.TypeError
					| _ => raise Type.TypeError (* T-APP *)
			end
		| Unit => Type.Unit (* T-UNIT *)
		| Address l => Type.Ref(storeTyping l) (* T-LOC *)
		| Ref t1 =>
			Type.Ref check(context, storeTyping, t1) (* T-REF *)
		| Deref t1 => case check(context, storeTyping, t1) of =
			Type.Ref t11 => t11
			| _ => raise Type.TypeError (* T-DEREF *)
		| Assign (t1, t2) =>
			let val ty2 = check(context, storeTyping, t2)
					ty1 = check(context, storeTyping, t1)
			in case of ty1 =
				Type.Ref ty3 =>
					if ty3 = ty2
					then Type.Unit
					else raise Type.TypeError
				| _ => raise Type.TypeError
			end (* T-ASSIGN *)

	(* to simplify the interface of the eval
	 * function, we can make the heap global, 
	 * instead of an argument to this function.
	 *)
	exception BadAddress

	structure Heap = struct

		val counter = ref 0
		fun fresh () =
		    let val n = !counter
		        val _ = counter := !counter + 1
		    in String.concat ["x_", Int.toString n]
		    end
    
		type heap = (string -> t) ref

		val heap: heap = ref (fn _ => raise BadAddress)

		fun alloc (t) = 
		    let val newAddress = fresh ()
		        val _ = heap := (fn y =>
		                           if y = newAddress
		                           then t
		                           else (!heap) y)
		    in  newAddress
		    end

		fun lookup (x) = (!heap) x

		fun update (x, t) = 
		    heap := (fn y =>
		                if y=x
		                then t
		                else (!heap) y)
    
		end (* structure Heap *)

	fun eval t = raise Todo

	fun pp t = case t of
	    Ref t => (print "ref "; pp t)
	    | Deref t => (print "!"; pp t)
	    | Assign (t1, t2) => (pp t1; print " := "; pp t2)
	    | Address x => print x
	    | Unit => print "()"
	    | _ => raise Todo

	fun evalAll t =
	    (let val t' = (eval t)
	        val _ = pp t'
	        val _ = print "\n"
	    in evalAll t'
	    end) handle NoRule => t 

	(* unit test *)

	val t1 = Ref Unit
	val ty1 = typeCheck (t1)
	val _ = print (Type.toString ty1)

	val t2 = Abs ("x", Type.Ref Type.Unit, Deref (Var "x"))

	val ty2 = typeCheck t2
	val _ = print (Type.toString ty2)

	val t3 = App (t2, t1)
	val ty3 = typeCheck t3
	val _ = print (Type.toString ty3)

	val _ = print "t4\n\n\n"
	val t4 = Assign(Ref Unit, Deref (Ref Unit))
	val _ = evalAll t4

end (* structure Reference *)
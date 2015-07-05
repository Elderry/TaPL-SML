structure Reference:REFERENCE = struct

	structure Type = struct

		datatype t =
			Bool
			| Unit
			| Fun of t * t
			| Ref of t

		fun equals(t1, t2) = case (t1, t2) of
		    (Bool, Bool) => true
	      	| (Fun(s1, s2), Fun(s3, s4)) =>
	      		equals(s1, s3)
	      		andalso equals(s2, s4)
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
	exception BadAddress
	exception NoRule

	(* to simplify the interface of the eval
	 * function, we can make the heap global, 
	 * instead of an argument to this function.
	 *)
	structure Heap = struct

		val counter = ref 0
		fun fresh() =
		    let val n = !counter
		        val _ = counter := !counter + 1
		    in String.concat ["x_", Int.toString n]
		    end
    
		type heap = (string -> t) ref
		type typing = (string -> Type.t) ref

		val heap: heap = ref(fn _ => raise BadAddress)
		val types: typing = ref(fn _ => raise BadAddress)

		fun alloc(t) =
		    let val newAddress = fresh()
		        val _ = heap := (
		        	fn y =>
                        if y = newAddress
                        then t
		                else (!heap) y)
		        val _ = types := (
		        	fn y =>
		        		if y = newAddress
		        		then typeCheck(t)
		        		else (!types) y)
		    in  newAddress
		    end

		fun lookup(x) = (!heap) x
		fun getType(x) = (!types) x

		fun update (x, t) = 
		    heap := (
		    	fn y =>
	                if y = x
	                then t
	                else (!heap) y)
    
	end (* structure Heap *)

	fun check(context, t) = case t of
		True => Type.Bool
		| False => Type.Bool
		| If (t1, t2, t3) =>
			let val ty1 = check(context, t1)
				val ty2 = check(context, t2)
				val ty3 = check(context, t3)
			in
				if
					Type.equals(Type.Bool, ty1)
					andalso Type.equals(ty2, ty3)
				then ty2
				else raise Type.TypeError
			end
		| Var x => context x (* T-VAR *)
		| Abs (x, ty, t2) =>
			check(fn y => if y = x then ty else context y, t2) (* T-ABS *)
		| App (t1, t2) =>
			let val ty2 = check(context, t2)
			in
				case check(context, t1) of
					Type.Fun (ty1, ty3) =>
						if Type.equals(ty1, ty2)
						then ty3
						else raise Type.TypeError
					| _ => raise Type.TypeError (* T-APP *)
			end
		| Unit => Type.Unit (* T-UNIT *)
		| Address l => Type.Ref(Heap.getType l) (* T-LOC *)
		| Ref t1 =>
			Type.Ref(check(context, t1)) (* T-REF *)
		| Deref t1 => (
			case check(context, t1) of
				Type.Ref ty1 => ty1
				| _ => raise Type.TypeError) (* T-DEREF *)
		| Assign(t1, t2) =>
			let val ty2 = check(context, t2)
				val	ty1 = check(context, t1)
			in case ty1 of
				Type.Ref ty3 =>
					if Type.equals(ty3, ty2)
					then Type.Unit
					else raise Type.TypeError
				| _ => raise Type.TypeError
			end (* T-ASSIGN *)

	fun typeCheck(t:t):Type.t = check(
		fn _ => raise Type.TypeError,
		t)

	fun isValue t = case t of
		True => true
		| False => true
		| Abs (_, _, _) => true
		| Unit => true
		| Address _ => true
		| _ => false

	(* Substitute x with v in t *)
	fun substitute(x, v, t) = case t of
    	Var x1 =>
            if x = x1
            then v
            else t
    	| Abs(y, ty, t1) => Abs(y, ty, substitute(x, v, t1))
    	| App(t1, t2) =>
            App(
                substitute(x, v, t1),
                substitute(x, v, t2))
        | Ref t1 => Ref(substitute(x, v, t1))
        | Deref t1 => Deref(substitute(x, v, t1))
        | Assign (t1, t2) =>
	        Assign(
	        	substitute(x, v, t1),
	        	substitute(x, v, t2))
	    | _ => t

	fun eval t = case t of
		If (True, t2, _) => t2
        | If (False, _, t3) => t3
        | If (t1, t2, t3) => If (eval t1, t2, t3)
		| App (t1, t2) =>
			if isValue t1
			then case t1 of
				Abs(x, ty1, t12) =>
					if isValue t2
					then substitute(x, t2, t12) (* E-APPABS *)
					else App(t1, eval t2) (* E-APP2 *)
				| _ => App(t1, eval t2) (* E-APP2 *)
			else App(eval t1, t2) (* E-APP1 *)
		| Ref t1 =>
			if isValue t1
			then Address (Heap.alloc(t1)) (* E-REFV *)
			else Ref(eval t1) (* E-REF *)
		| Deref t1 => (
			case t1 of
				Address l => Heap.lookup(l) (* E-DEREFLOC *)
				| _ => Deref (eval t1)) (* E-DEREF *)
		| Assign (t1, t2) =>
			if isValue t1
			then
				if isValue t2
				then case t1 of
					Address l =>
						let val _ = Heap.update(l, t2)
						in Unit (* E-ASSIGN *)
						end
					| _ => raise NoRule
				else Assign(t1, eval t2) (* E-ASSIGN2 *)
			else Assign(eval t1, t2) (* E-ASSIGN1 *)
		| _ => raise NoRule

	fun pp t = case t of
	    Ref t => (print "ref "; pp t)
	    | Deref t => (print "!"; pp t)
	    | Assign(t1, t2) => (pp t1; print " := "; pp t2)
	    | Address x => print x
	    | Unit => print "()"
	    | Var x => print x
        | Abs(x, ty, t) => (
            print "\\lambda ";
            print x;
            print ":";
            print (Type.toString ty);
            print ".(";
            pp t;
            print ")")
        | App(e1, e2) => (
            print "(";
            pp e1;
            print ") ";
            print "(";
            pp e2;
            print ")")
        | True => print "true"
        | False => print "false"
        | If (t1, t2, t3) => (
        	print "If(" ;
        	pp t1;
        	print ", ";
        	pp t2;
        	print ", ";
        	pp t3;
        	print ")")

	fun evalAll t =
	    (let val t' = (eval t)
	        val _ = pp t'
	        val _ = print "\n"
	    in evalAll t'
	    end) handle NoRule => t 

	(* unit test *)

	val t1 = Ref Unit
	val ty1 = typeCheck(t1)
	val _ = print(Type.toString ty1)

	val t2 = Abs("x", Type.Ref Type.Unit, Deref(Var "x"))

	val ty2 = typeCheck t2
	val _ = print(Type.toString ty2)

	val t3 = App(t2, t1)
	val ty3 = typeCheck t3
	val _ = print(Type.toString ty3)

	val _ = print "t4\n\n\n"
	val t4 = Assign(Ref Unit, Deref(Ref Unit))
	val _ = evalAll t4

end (* structure Reference *)
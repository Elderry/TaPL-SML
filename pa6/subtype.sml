structure Subtype:SUBTYPE = struct

    structure Type = struct

        datatype t =
            Record of (string * t) list
            | Top
            | String
            | Arrow of t * t

        fun isSub(t1, t2) = case t2 of
            Record l2 => (
            	case t1 of
            		Record l1 => isSubRcd(l1, l2)
            		| _ => false)
            | Top => true (* S-TOP *)
            | String => (
                case t1 of
                    String => true
                    | _ => false)
            | Arrow(T1, T2) => (
                case t1 of
                    Arrow(S1, S2) =>
                        if isSub(T1, S1) andalso isSub(S2, T1)
                        then true
                        else false
                    | _ => false) (* S-ARROW *)

        and isSubRcd(l1, l2) = case l2 of
        	[] => true
        	| (name, T)::l2' => foundInRcd(name, T, l1) andalso isSubRcd(l1, l2')

        and foundInRcd(name, T, l) = case l of
        	[] => false
        	| (name', T')::l' =>
        		if name = name' andalso isSub(T, T')
        		then true
        		else foundInRcd(name, T, l')

	    fun appendRcd(r1, r2) =
	    	let val Record l1 = r1
	    		val Record l2 = r2
	    	in Record(l1@l2)
	    	end

        fun pp t = case t of
        	Record l =>
	        	let fun ppfields l =
	                case l of
	                    [] => ()
	                    | (name, t)::xs => (
	                        print name;
	                        print " : ";
	                        pp t;
	                        print "; ";
	                        ppfields xs)
	            in (
	                print "{";
	                ppfields l;
	                print "}")
	            end
	        | Top => print "Top"
            | String => print "String"
            | Arrow(t1, t2) => (
            	pp t1;
            	print " -> ";
            	pp t2)
             
    end (* structure Type *)

    datatype t =
        Record of (string * t) list
        | Proj of t * string
        | String of string
        | Var of string
        | Abs of string * Type.t * t
        | App of t * t

    exception TypeError of string

    fun checkProj(context, l, name) =
    	case l of
    		[] => raise TypeError (String.concat["Record doesn't has Projection: ", name])
    		| (name', T)::l' =>
    			if name' = name
    			then T
    			else checkProj(context, l', name)

    and check(context, t) = case t of
        Record l => (
        	case l of
        		[] => Type.Record []
        		| (name, t')::l' =>
        			Type.appendRcd(
        				Type.Record [(name, check(context, t'))],
        				check(context, Record l')))
        | Proj(t, name) => (
        	case check(context, t) of
	        	Type.Record l => checkProj(context, l, name)
	        	| _ => raise TypeError "Only Record has Projection")
        | String _ => Type.String
        | Var x => context x (* T-VAR *)
        | Abs(x, T1, t2) =>
            Type.Arrow(
                T1,
                check(fn y => if y = x then T1 else context y, t2)) (* T-ABS *)
        | App(t1, t2) =>
        	let val T1 = check(context, t1)
        		val T2 = check(context, t2)
        	in case T1 of
        		Type.Arrow(T11, T12) =>
        			if Type.isSub(T2, T11)
        			then T12
        			else raise TypeError "Parameter with wrong type"
        		| _ => raise TypeError "Function with wrong type"
    		end (* T-APP *)

    fun typeCheck t = check(fn x => raise TypeError "var not found", t)

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
        | Proj(t, name) => (
            pp t;
            print ".";
            print name)
        | String s => print s
        | Var x => print x
        | Abs(x, ty, t) => (
            print "\\lambda ";
            print x;
            print ": ";
            Type.pp ty;
            print ".";
            pp t)
        | App(t1, t2) => (
            print "(";
            pp t1;
            print ") (";
            pp t2;
            print ")")

    (* unit test *)
    val t =
        App(
            Abs(
                "x",
                Type.Record[("name" , Type.String)],
                Proj(Var "x" , "name")),
            Record[
                ("age", String "20"),
                ("name", String "Bob")])

    val _ = (
    	print "\nTerm:\n";
        pp t;
        print "\n")
    
    val ty = typeCheck t

    val _ = (
    	print "\nTerm's type:\n";
    	Type.pp ty;
    	print "\n\n")

end
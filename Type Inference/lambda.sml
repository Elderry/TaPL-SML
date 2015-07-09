structure Lambda: LAMBDA =
struct

structure Type =
struct
datatype t
  = Bool
  | Var of string
  | Arrow of t * t

fun map (f, t) =
    case t
     of Bool => Bool
      | Var x => f x
      | Arrow (t1, t2) =>
        Arrow (map (f, t1), map(f, t2))

fun pp t =
    case t
     of Bool => print "Bool"
      | Var x => print x
      | Arrow (t1, t2) =>
        (print "("
       ; pp t1
       ; print "->"
       ; pp t2
       ; print ")")

(* [x|->t1]t2 *)
fun subst (x, t1, t2) =
    case t2
     of Bool => Bool
      | Var y =>
        if x=y 
        then t1
        else t2
      | Arrow (s1, s2) =>
        Arrow (subst (x, t1, s1), subst(x, t1, s2))
             
end (* structure Type *)

exception Todo

structure Constraint =
struct

type t = (Type.t * Type.t) list

fun empty (): t = []

fun solve (t) = raise Todo

fun pp t = List.app (fn (x, y) => 
                        (Type.pp x
                       ; print "="
                       ; Type.pp y
                       ; print "; "))
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
fun freshTypeVar () =
    let val n = !counter
        val _ = counter := (n+1)
    in  String.concat ["X_", Int.toString n]
    end


fun check (env: string -> Type.t, t): Type.t * Constraint.t = 
    raise Todo

fun typeCheck t = 
    let val (ty, c) = check (fn x => raise TypeError "var not found", t)
        val _ = Constraint.pp c
        val _ = Type.pp ty
        val map = Constraint.solve (c)
        val ty = Type.map (map, ty)
        val _ = Type.pp ty
    in  ty
    end

fun pp t =
    case t 
     of True => print "true"
      | False => print "false"
      | If (e1, e2, e3) =>
        (print "if "
       ; pp e1
       ; print " then "
       ; pp e2
       ; print " else "
       ; pp e3)
      | Var x => print x
      | Abs (x, t) =>
        (print "\\lambda "
       ; print x
       ; print "."
       ; pp t)
      | App (t1, t2) =>
        (print "("
       ; pp t1
       ; print ") ("
       ; pp t2
       ; print ")")



(* unit test *)
val t = App (Abs ("x", Var "x")
           , True)
val _ = (pp t; print "\n")
val ty = typeCheck t

end

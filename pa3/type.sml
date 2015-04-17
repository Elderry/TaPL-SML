structure Type = struct

datatype ty
  = Bool
  | Fun of ty * ty
           
datatype t
  = True
  | False
  | If of t * t * t
  | Var of string
  | Abs of string * ty * t
  | App of t * t
       
fun tyEquals (ty1, ty2) = raise Todo
        
fun typeCheck t = raise Todo

val e = App (Abs ("x", Fun (Bool, Bool), Var "x")
           , (Abs ("x", Bool, Var "x")))

val _ = typeCheck e

end
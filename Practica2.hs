module Sintax where

type Identifier = String
    

{--
 -- Sintaxis Practica1
 -- Para su Practica 2 deben modificar esto (es lo unico que necesitan de su P1)
 -- Agregar y eliminar las cosas que sean necesarias segun la descripcion que se
 -- dio en la especificacionde la practica.
 --}
data Expr = Var Identifier | I Int | B Bool
         | Add Expr Expr | Mul Expr Expr
         | Succ Expr | Pred Expr -- | Neg Expr -- Hint 1: Eliminar el constructor Neg
         | And Expr Expr | Or Expr Expr
         | Not Expr | Iszero Expr
         | If Expr Expr Expr
         | Let Expr Expr -- Hint 2: El Let pueden implementarlo como en la especificacion de la practica 1 o como en la 2
         | Fn Identifier Expr  -- Hint 3: Este constructor puede cambiar de nombre (Fn)
           deriving (Show, Eq)

-- type Identifier = Int
data Type = T Identifier
        | Integer | Boolean
        | Arrow Type Type

type Ctxt =  [(Identifier, Type)]

type Constraint = [(Type, Type)]

tvars :: Type -> [Identifier]
tvars x = case x of 
        T i -> [i]
        Integer -> []
        Boolean -> []
        Arrow e1 e2 -> tvars e1 ++ tvars e2


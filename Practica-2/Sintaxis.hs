module Sintaxis where

type Identifier = String
    

{--
 -- Sintaxis Practica1
 -- Para su Practica 2 deben modificar esto (es lo unico que necesitan de su P1)
 -- Agregar y eliminar las cosas que sean necesarias segun la descripcion que se
 -- dio en la especificacionde la practica.
 --}
data Expr = Var Identifier | I Int | B Bool
         | Add Expr Expr | Mul Expr Expr
         | Succ Expr | Pred Expr
         | And Expr Expr | Or Expr Expr
         | Not Expr | Iszero Expr
         | If Expr Expr Expr
         | App Expr Expr
         | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
         | Let Identifier Expr Expr 
         | Fn Identifier Expr  
           deriving (Show, Eq)

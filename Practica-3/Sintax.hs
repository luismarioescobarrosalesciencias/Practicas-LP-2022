module Sintax where

type Identifier = String
    

{--
 -- Sintaxis Practica2
 -- Para su Practica 3 deben extender esta sintaxis
 -- Agregar y eliminar las cosas que sean necesarias segun la descripcion que se
 -- dio en la especificacionde la practica.
 --}
data Expr = Var Identifier | I Int | B Bool
          | Add Expr Expr | Mul Expr Expr
          | Succ Expr | Pred Expr
          | And Expr Expr | Or Expr Expr
          | Not Expr | Iszero Expr
          | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
          | If Expr Expr Expr
          | Let Identifier Expr Expr
          | Fn Identifier Expr
          | App Expr Expr
          deriving (Show, Eq)

{--
    Igual deben adecuar las funciones realizas en la practica 1 para ajustarse a esta sintaxis
--}




module Sintax where

type Identifier = String 
    
-- Sintaxis completa
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
          | L Int
          | Alloc Expr
          | Dref Expr
          | Assign Expr
          | Void
          | Seq Expr Expr
          | While Expr Expr
          deriving (Show, Eq)

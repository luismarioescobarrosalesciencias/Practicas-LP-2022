module Practica04 where 
import Sintax

--Identificador.
type Identifier = String

-- Definicion de marcos
data Frame = AddFL Expr | AddFR Expr
            | MulFL Expr | MulFR Expr
            | SuccF | PredF
            | AndFL Expr | AndFR Expr 
            | OrFL Expr | OrFR Expr
            | NotF | IszeroF
            | LtFL Expr | LtFR Expr
            | GtFL Expr | GtFR Expr 
            | EqFL Expr | EqFR Expr
            | IfF Expr Expr
            | LetM Identifier Expr
            | AppFL Expr | AppFR Expr
            | AllocF
            | DrefF
            | AssignFL Expr | AssignFR Expr
            | SeqF Expr
            deriving (Eq, Show)

--Definicion de pila de marcos
data Stack = Empty
           | S Frame Stack
           deriving Show

-- Alias para direcciones de memoria.
type Address = Int
{-- Alias para valores. Aunque por implementacion se podria poner cualquier expresion, se espera solo
sean valores. --}

type Value = Expr

type Cell = ( Address , Value )

type Memory = [ Cell ]

data State = E Stack Memory Expr
           | R Stack Memory Expr
           | P Stack Memory Expr
           deriving Show


-- Ejemplos de la funcion eval1
eval1 :: State -> State
--- VALUES
eval1 (E s m e@(I n)) = R s m e
eval1 (E s m e@(B b)) = R s m e
eval1 (E s m e@(Fn x exp)) = R s m e
--- ADD
eval1 (E s m (Add e1 e2)) = E (S (AddFL e2) s) m e1
eval1 (R (S (AddFL e2) s) m v) = E (S (AddFR v) s) m e2
eval1 (R (S (AddFR (I v1)) s) m (I v2)) = R s m (I (v1+v2))
--- MUL
eval1 (E s m (Mul e1 e2)) = E (S (MulFL e2) s) m e1
eval1 (R (S (MulFL e2) s) m v) = E (S (MulFR v) s) m e2
eval1 (R (S (MulFR (I v1)) s) m (I v2)) = R s m (I (v1+v2))
--- SUCC
eval1 (E s m (Succ e)) = E (S SuccF s) m e
eval1 (R (S SuccF s) m (I v)) = R s m (I (v+1))
--- WHILE
eval1 (E s m w@(While f e)) = E s m (If f (Seq e w) Void)

{-- 
Ejemplo evaluacion de un valor en una seq
Seq (Add 1 2) (While ...)
Seq 3 (While ...)
Seq () (While ...)
While ...

Ejemplo de evaluacion de un alloc en la maquina K donde M es la memoria inicial
P > M, Alloc (Prod 7 8)
AllocF; P > M, Prod 7 8
(ProdFL 8) ; Alloc F ; P > M, 7
(ProdFL 8) ; Alloc F ; P < M, 7
(ProdFR 7) ; Alloc F ; P < M, 8
Alloc F ; P < M, 56
P < (M, (1,56)), L 1
--}

module Practica3 where 
import Sintax

-- Alias para direcciones de memoria.
type Address = Int
{- - Alias para valores. Aunque por implementacion se podria poner cualquier expresion, se espera solo
sean valores. - -}
type Value = Expr
type Cell = ( Address , Value )
type Memory = [ Cell ]

newAddress :: Memory -> Expr
newAddress [] = (L 0) 
newAddress xs = L (newAddressAux xs 0)

newAddressAux :: Memory -> Int -> Int
newAddressAux [] n = n
newAddressAux (x:xs) n = if (estaContenido (x:xs) n) then newAddressAux (x:xs) (n+1) else n

estaContenido :: Memory -> Int -> Bool
estaContenido [] n = False
estaContenido (x:xs) n = if (fst x) == n then True else estaContenido xs n

access :: Address -> Memory -> Maybe Value
access n [] = Nothing
access n (x:xs) = if (estaContenido (x:xs) n) then Just (accessAux n xs) else Nothing

accessAux :: Address -> Memory -> Value
--accessAux n [] = error "no debería pasar esto"
accessAux n (x:xs) = if (fst x) == n then snd x else accessAux n xs

update :: Cell -> Memory -> Maybe Memory
update c [] = Nothing
update c (x:xs) = if (estaContenido (x:xs) (fst c)) then Just (updateAux c (x:xs)) else Nothing

updateAux :: Cell -> Memory -> Memory
--updateAux c [] = error "no debería pasar esto"
updateAux c (x:xs) = if (fst c) == (fst x) then (c:xs) else [x] ++ (updateAux c xs)

frVars :: Expr -> [ Identifier ]
frVars (Var s) = [s]
frVars (I _) = []
frVars (B _) = []
frVars (Add e1 e2) = frVars e1 ++ frVars e2
frVars (Mul e1 e2) = frVars e1 ++ frVars e2
frVars (Succ e) = frVars e
frVars (Pred e) = frVars e
frVars (And e1 e2) = frVars e1 ++ frVars e2
frVars (Or e1 e2) = frVars e1 ++ frVars e2
frVars (Not e) = frVars e
frVars (Iszero e) = frVars e
frVars (Lt e1 e2) = frVars e1 ++ frVars e2
frVars (Gt e1 e2) = frVars e1 ++ frVars e2
frVars (Eq e1 e2) = frVars e1 ++ frVars e2
frVars (If e1 e2 e3) = frVars e1 ++ frVars e2 ++ frVars e3
frVars (Let i e1 e2) = filter (/= i) (frVars e2) -- ???
frVars (Fn i e) = filter (/=i) (frVars e)
frVars (App e1 e2) = frVars e1 ++ frVars e2
frVars (L i) = []
frVars (Alloc e) = frVars e
frVars (Assign e1 e2) = frVars e1 ++ frVars e2
frVars (Void) = []
frVars (Seq e1 e2) = frVars e1 ++ frVars e2
frVars (While e1 e2) = frVars e1 ++ frVars e2

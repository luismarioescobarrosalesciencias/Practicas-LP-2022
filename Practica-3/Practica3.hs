module Practica3 where 
import Sintax

data Expr =  L Int
         | Alloc Practica3.Expr
         | Dref Practica3.Expr
         | Assign Practica3.Expr Practica3.Expr
         | Void
         | Seq Practica3.Expr Practica3.Expr
         | While Sintax.Expr Sintax.Expr
         deriving (Show, Eq)

-- Alias para direcciones de memoria.
type Address = Int
{- - Alias para valores. Aunque por implementacion se podria poner cualquier expresion, se espera solo
sean valores. - -}
type Value = Sintax.Expr
type Cell = ( Address , Value )
type Memory = [ Cell ]

newAddress :: Memory -> Practica3.Expr
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


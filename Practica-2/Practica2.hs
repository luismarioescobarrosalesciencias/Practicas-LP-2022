module Practica2 where 
import Sintaxis 

type Identifier= Int

data Type = T Practica2.Identifier
        | Integer | Boolean
        | Arrow Type Type

instance Show Type where 
        show e = case e of 
                (T i) -> "T " ++ (show i)
                (Arrow e1 e2) -> show e1 ++ " -> " ++ show e2
                Integer -> "int"
                Boolean -> "bool"

type Ctxt = [(Sintaxis.Identifier, Type)]
type Substitution = [(Practica2.Identifier, Type)]
type Constraint = [(Type, Type)]

tvars :: Type -> [Practica2.Identifier]
tvars x = case x of 
        T i -> [i]
        Arrow e1 e2 -> quitaVarsRepetidos (tvars e1 ++ tvars e2)
        Integer -> []
        Boolean -> [] 

-- Para quitar en tvars los nuemeros repetidos.
quitaVarsRepetidos  :: (Eq c) => [c] -> [c]
quitaVarsRepetidos [] = []
quitaVarsRepetidos (x:xs) = x : quitaVarsRepetidos(filter (/= x) xs) 



subst :: Type -> Substitution -> Type
subst t [] = t
subst (T n) ((i, t):xs) = if n==i
                          then subst t xs 
                          else subst (T n) xs
subst (Arrow t1 t2) xs = Arrow (subst t1 xs) (subst t2 xs)
subst t _ = t

comp :: Substitution -> Substitution -> Substitution
comp s1 s2 = noDup ((map (\x -> (fst x, subst (snd x) s2)) s1) ++ s2)

--Quita duplicados de una lista
noDup :: (Eq a) => [(a, b)] -> [(a, b)]
noDup (x:xs) = x : noDup (filter (\y -> (fst y) /= (fst x)) xs)
noDup [] = [] 
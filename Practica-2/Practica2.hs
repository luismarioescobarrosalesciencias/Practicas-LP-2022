module Practica2 where 
import Sintaxis 

type Identifier = Int

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

fresh :: [Type] -> Type
fresh [] = T 0
fresh x = T (freshAux (x) (0))


-- Da un valor int distinto a los valores ints de los tipos dados en la lista Type.
freshAux :: [Type] -> Int -> Int
freshAux [] y = y
freshAux (x:xs) y = if (estaContenido ((x:xs)) (y)) then freshAux ((x:xs)) (y + 1) else y

-- Para quitar en tvars los nuemeros repetidos.
quitaVarsRepetidos  :: (Eq c) => [c] -> [c]
quitaVarsRepetidos [] = []
quitaVarsRepetidos (x:xs) = x : quitaVarsRepetidos(filter (/= x) xs) 

-- Checa si un int esta contenido en el valor de un tipo.
estaContenido :: [Type] -> Int -> Bool
estaContenido [] y = False
estaContenido (x:xs) y = if(obtenInt (x) == y) then True else estaContenido (xs) (y) 

-- Obtiene un int de un tipo T n 
obtenInt :: Type -> Practica2.Identifier
obtenInt x = case x of 
        T i -> i       

rest :: ([Type] , Expr) -> ([Type] , Ctxt , Type, Constraint)
rest (xs, (Var x)) = (((tn):xs), [(x, tn)], (tn) , [])
                        where tn = fresh xs
rest (xs, (I n)) = (Integer:xs,[],Integer,[])
rest (xs, (B b)) = (Boolean:xs, [], Boolean, [])
rest (xs,(Add e1 e2)) = (xs2, g1 ++ g2, Integer, rf)
                        where (xs1,g1,t1,r1) = (rest (xs,e1)) 
                              (xs2,g2,t2,r2) = (rest (xs1,e2))
                              rs = [(tn1,tn2) | (x,tn1) <- g1, (y,tn2) <- g2, x ==y]
                              re = [(t1,Integer),(t2,Integer)]
                              rf = r1++r2++rs++re
rest (xs,(Mul e1 e2)) = (xs2, g1 ++ g2, Integer, rf)
                        where (xs1,g1,t1,r1) = (rest (xs,e1)) 
                              (xs2,g2,t2,r2) = (rest (xs1,e2))
                              rs = [(tn1,tn2) | (x,tn1) <- g1, (y,tn2) <- g2, x ==y]
                              re = [(t1,Integer),(t2,Integer)]
                              rf = r1++r2++rs++re
rest (xs,(And e1 e2)) = (xs2, g1 ++ g2, Boolean, rf)
                        where (xs1,g1,t1,r1) = (rest (xs,e1)) 
                              (xs2,g2,t2,r2) = (rest (xs1,e2))
                              rs = [(tn1,tn2) | (x,tn1) <- g1, (y,tn2) <- g2, x ==y]
                              re = [(t1,Boolean),(t2,Boolean)]
                              rf = r1++r2++rs++re    
rest (xs,(Or e1 e2)) = (xs2, g1 ++ g2, Boolean, rf)
                        where (xs1,g1,t1,r1) = (rest (xs,e1)) 
                              (xs2,g2,t2,r2) = (rest (xs1,e2))
                              rs = [(tn1,tn2) | (x,tn1) <- g1, (y,tn2) <- g2, x ==y]
                              re = [(t1,Boolean),(t2,Boolean)]
                              rf = r1++r2++rs++re 
rest (xs,(Lt e1 e2)) = (xs2, g1 ++ g2, Boolean, rf)
                        where (xs1,g1,t1,r1) = (rest (xs,e1)) 
                              (xs2,g2,t2,r2) = (rest (xs1,e2))
                              rs = [(tn1,tn2) | (x,tn1) <- g1, (y,tn2) <- g2, x ==y]
                              re = [(t1,Boolean),(t2,Boolean)]
                              rf = r1++r2++rs++re    
rest (xs, Gt e1 e2) = (xs2, g1 ++ g2, Boolean, rf)
                        where (xs1,g1,t1,r1) = (rest (xs,e1)) 
                              (xs2,g2,t2,r2) = (rest (xs1,e2))
                              rs = [(tn1,tn2) | (x,tn1) <- g1, (y,tn2) <- g2, x ==y]
                              re = [(t1,Boolean),(t2,Boolean)]
                              rf = r1++r2++rs++re                                                        
rest (xs, Iszero e) = (xs1, g1 , Boolean, r1)
                        where (xs1, g1, t1, r1) = (rest (xs, e))                                                                                     
rest (xs, (Succ e)) = (xs1, g1 , Integer, r1)
                        where (xs1, g1, t1, r1) = (rest (xs, e))
rest (xs, (Pred e)) = (xs1, g1 , Integer, r1)
                        where (xs1, g1, t1, r1) = (rest (xs, e))
rest (xs, (Not e)) = (xs1, g1 , Boolean, r1)
                        where (xs1, g1, t1, r1) = (rest (xs, e))

                  
                           

subst :: Type -> Substitution -> Type
subst t [] = t
subst (T n) ((i, t):xs) = if n==i
                          then subst t xs 
                          else subst (T n) xs
subst (Arrow t1 t2) xs = Arrow (subst t1 xs) (subst t2 xs)
subst t _ = t

substCons :: Substitution -> Constraint -> Constraint
substCons _ [] = []
substCons c ((t , s):xs) =  [((subst t c) , (subst s c))] ++ substCons c xs

comp :: Substitution -> Substitution -> Substitution
comp s1 s2 = noDup ((map (\x -> (fst x, subst (snd x) s2)) s1) ++ s2)

--porque en lab se vio que se le pasaba un unificador (lista de substituciones) y devolcia unificador
compLis :: [Substitution] -> Substitution -> [Substitution]
compLis [] _ = []
compLis (x:xs) s = [comp x s] ++ compLis xs s

--Quita duplicados de una lista
noDup :: (Eq a) => [(a, b)] -> [(a, b)]
noDup (x:xs) = x : noDup (filter (\y -> (fst y) /= (fst x)) xs)
noDup [] = [] 

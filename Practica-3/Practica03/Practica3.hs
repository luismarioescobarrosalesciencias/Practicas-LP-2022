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
newAddress xs = if repetidos xs then error"Memory Corrupted" else  L (newAddressAux xs 0)

repetidos :: Memory -> Bool
retetidos [] = False
repetidos (x:[]) = False
repetidos (x:xs) = if (estaContenido xs (fst x))then True else repetidos (xs)

newAddressAux :: Memory -> Int -> Int
newAddressAux [] n = n
newAddressAux (x:xs) n = if (estaContenido (x:xs) n) then newAddressAux (x:xs) (n+1) else n

estaContenido :: Memory -> Int -> Bool
estaContenido [] n = False
estaContenido (x:xs) n = if (fst x) == n then True else estaContenido xs n

access :: Address -> Memory -> Maybe Value
access n [] = Nothing
access n (x:xs) = if repetidos (x:xs) then error"Memory Corrupted"
                  else if (estaContenido (x:xs) n) then Just (accessAux n (x:xs)) else Nothing

accessAux :: Address -> Memory -> Value
accessAux n (x:xs) = if (fst x) == n then snd x else accessAux n xs

update :: Cell -> Memory -> Maybe Memory
update c [] = Nothing
update c (x:xs) = if repetidos (x:xs) then error"Memory Corrupted"
                   else if (estaContenido (x:xs) (fst c)) then Just (updateAux c (x:xs)) else Nothing

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
frVars (Fn i e) = filter (/=i) (frVars e) -- ??
frVars (App e1 e2) = frVars e1 ++ frVars e2
frVars (L i) = []
frVars (Alloc e) = frVars e
frVars (Assign e1 e2) = frVars e1 ++ frVars e2
frVars (Void) = []
frVars (Seq e1 e2) = frVars e1 ++ frVars e2
frVars (While e1 e2) = frVars e1 ++ frVars e2

subst :: Expr -> Substitution -> Expr
subst (Var x) s = if x == (fst s)
                       then (snd s)
                       else (Var x)
subst (I x) _ = (I x)
subst (B b) _ = (B b)
subst (Add e1 e2) s = Add (subst e1 s) (subst e2 s)
subst (Mul e1 e2) s = Mul (subst e1 s) (subst e2 s)
subst (Succ e) s = Succ (subst e s)
subst (Pred e) s = Pred (subst e s)
subst (And e1 e2) s = And (subst e1 s) (subst e2 s)
subst (Or e1 e2) s = Or (subst e1 s) (subst e2 s)
subst (Not e) s = Not (subst e s)
subst (Iszero e) s = Iszero (subst e s)
subst (Lt e1 e2) s = Lt (subst e1 s) (subst e2 s)
subst (Gt e1 e2) s = Gt (subst e1 s) (subst e2 s)
subst (Eq e1 e2) s = Eq (subst e1 s) (subst e2 s)
subst (If e1 e2 e3) s = If (subst e1 s) (subst e2 s) (subst e3 s)
subst (Let i e1 e2) s = if (fst s) `elem` (frVars e2)
                        then Let i e1 e2
                        else Let i (subst e1 s) (subst e2 s)
subst (App e1 e2) s = App (subst e1 s) (subst e2 s)
subst (L i) s = (L i)
subst (Alloc e) s = Alloc (subst e s)
subst (Assign e1 e2) s = Assign (subst e1 s) (subst e2 s)
subst (Void) s = Void
subst (Seq e1 e2) s = Seq (subst e1 s) (subst e2 s)
subst (While e1 e2) s = While (subst e1 s) (subst e2 s)

eval1 :: (Memory , Expr ) -> (Memory , Expr)
eval1 (mem, Var x) = (mem, Var x)
eval1 (mem, B x) = (mem, B x)
eval1 (mem, I x) = (mem, I x)
eval1 (mem, (Add e1 e2)) = case (e1, e2) of
                           ((I e1), (I e2)) -> (mem, I (e1+e2))
                           (s , c) -> (mem, Add e1 e2) 
eval1 (mem, (Mul e1 e2)) = case (e1, e2) of
                           ((I e1), (I e2)) -> (mem, I (e1*e2))
                           (s , c) -> (mem, Mul e1 e2)
eval1 (mem, Succ e) = case e of
                      (I x) -> (mem, I (x+1))
                      (_) ->  (mem, Succ e)
eval1 (mem, Pred e) = case e of
                      (I x) -> (mem, I (x-1))
                      (_) -> (mem, Pred e)
eval1 (mem, (And e1 e2)) = case (e1, e2) of
                           ((B e1), (B e2)) -> (mem, B (e1 && e2))
                           (s , c) -> (mem, And e1 e2)
eval1 (mem, (Or e1 e2)) = case (e1, e2) of
                           ((B e1), (B e2)) -> (mem, B (e1 || e2))
                           (s , c) -> (mem, Or e1 e2)
eval1 (mem, Not e) = case e of
                    (B e) -> (mem, B (not e))
                    (_) -> (mem, Not e)
eval1 (mem, Iszero e) = case e of
                        (I e) -> if e == 0
                                then (mem, B True)
                                else (mem, B False)
                        (_) -> (mem, Iszero e)
eval1 (mem, Lt e1 e2) = case (e1, e2) of
                              (I e1, I e2) -> (mem, B (e1 < e2))  
                              (s , c) -> (mem, Lt e1 e2)  
eval1 (mem, Gt e1 e2) = case (e1, e2) of
                              (I e1, I e2) -> (mem, B (e1 > e2))
                              (s , c) -> (mem, Gt e1 e2)  
eval1 (mem, Eq e1 e2) = case (e1, e2) of
                              (I e1, I e2) -> (mem, B (e1 == e2))  
                              (s , c) -> (mem, Eq e1 e2)  
eval1 (mem, If e1 e2 e3) = case e1 of
                            (B True) -> (mem, e2)
                            (B False) -> (mem, e3) 
                            (s) -> (mem', (If (c)(e2)(e3)))
                            where (mem', c) = eval1 (mem, e1)   -- seria asi?   
eval1 (mem, Let i e1 (Fn x e2)) = case e1 of                    -- i esta bien colocado?
                                (B e1) -> (mem, (subst (e2) (x, (B e1))))
                                (I e1) -> (mem, (subst (e2) (x, (I e1))))
                                (s) -> (mem', (Let i (c)(Fn x e2)))
                                where (mem', c) = eval1 (mem, e1)
                                
eval1 (mem, Seq e1 e2) = case (e1, e2) of
                               (Void, s) -> (mem, s)
                               (s , c) -> (mem', Seq (d) (e2))
                                      where (mem', d) = eval1 (mem, e1)  
eval1 (mem, While e1 e2) = (mem, If (e1) (Seq (e2) (While e1 e2)) (Void))                                      


evals :: (Memory , Expr ) -> (Memory , Expr )
evals (mem, Var x) = (mem, Var x)
evals (mem, B x) = (mem, B x)
evals (mem, I x) = (mem, I x)
evals (mem, e@(Add e1 e2)) = case (e1, e2) of 
                           ((I x), (I y)) -> (mem, I (x+y))
                           (s, c) -> eval1 (memt, h)
                                  where (mem1, p) = eval1 (mem, s)
                                        (mem2, q) = eval1 (mem1, c)
                                        (memt, h) = eval1 (mem2, Add p q)
evals (mem, e@(Mul e1 e2)) = case (e1, e2) of 
                           ((I x), (I y)) -> (mem, I (x*y))
                           (s, c) -> eval1 (memt, h)
                                  where (mem1, p) = eval1 (mem, s)
                                        (mem2, q) = eval1 (mem1, c)
                                        (memt, h) = eval1 (mem2, Mul p q)
evals (mem, Succ e) = case e of
                      (I x) -> (mem, I (x+1))
                      (s) ->  eval1 (mem, Succ s)
evals (mem, Pred e) = case e of
                      (I x) -> (mem, I (x-1))
                      (s) ->  eval1 (mem, Pred s)
evals (mem, e@(And e1 e2)) = case (e1, e2) of 
                           ((B x), (B y)) -> (mem, B (x&&y))
                           (s, c) -> eval1 (memt, h)
                                  where (mem1, p) = eval1 (mem, s)
                                        (mem2, q) = eval1 (mem1, c)
                                        (memt, h) = eval1 (mem2, And p q)
evals (mem, e@(Or e1 e2)) = case (e1, e2) of 
                           ((B x), (B y)) -> (mem, B (x||y))
                           (s, c) -> eval1 (memt, h)
                                  where (mem1, p) = eval1 (mem, s)
                                        (mem2, q) = eval1 (mem1, c)
                                        (memt, h) = eval1 (mem2, Or p q)
evals (mem, Not e) = case e of
                      (B x) -> (mem, B (not x))
                      (s) ->  eval1 (mem, Not s)
evals (mem, Iszero e) = case e of
                      (I x) -> if x == 0 
                              then (mem, B True)
                              else (mem, B False)
                      (s) ->  eval1 (mem', p)
                              where (mem', p) = eval1 (mem, s)
evals (mem, Lt e1 e2) = case (e1, e2) of 
                           ((I x), (I y)) -> if x < y
                                             then (mem, B True)
                                             else (mem, B False)
                           (s, c) -> eval1 (memt, h)
                                  where (mem1, p) = eval1 (mem, s)
                                        (mem2, q) = eval1 (mem1, c)
                                        (memt, h) = eval1 (mem2, Lt p q)
evals (mem, Gt e1 e2) = case (e1, e2) of 
                           ((I x), (I y)) -> if x > y
                                             then (mem, B True)
                                             else (mem, B False)
                           (s, c) -> eval1 (memt, h)
                                  where (mem1, p) = eval1 (mem, s)
                                        (mem2, q) = eval1 (mem1, c)
                                        (memt, h) = eval1 (mem2, Gt p q)
evals (mem, Eq e1 e2) = case (e1, e2) of 
                           ((I x), (I y)) -> if x == y
                                             then (mem, B True)
                                             else (mem, B False)
                           (s, c) -> eval1 (memt, h)
                                  where (mem1, p) = eval1 (mem, s)
                                        (mem2, q) = eval1 (mem1, c)
                                        (memt, h) = eval1 (mem2, Eq p q)


evale :: Expr -> Expr
evale (Var x) = (Var x)
evale (B x) = (B x)
evale (I x) = (I x)
evale (e@(Add e1 e2)) = case evals ([(0, Void)], e) of
                        (mem, I s) -> I s
                        _ -> error "La expresion es erronea para Add" 
evale (e@(Mul e1 e2)) = case evals ([(0, Void)], e) of
                        (mem, I s) -> I s
                        _ -> error "La expresion es erronea para Mul" 
evale (e@(Succ e1)) = case evals ([(0, Void)], e) of
                        (mem, I s) -> I s
                        _ -> error "La expresion es erronea para Succ"
evale (e@(Pred e1)) = case evals ([(0, Void)], e) of
                        (mem, I s) -> I s
                        _ -> error "La expresion es erronea para Pred"    
evale (e@(And e1 e2)) = case evals ([(0, Void)], e) of
                        (mem, B s) -> B s
                        _ -> error "La expresion es erronea para And"   
evale (e@(Or e1 e2)) = case evals ([(0, Void)], e) of
                        (mem, B s) -> B s
                        _ -> error "La expresion es erronea para Or"                   
evale (e@(Not e1)) = case evals ([(0, Void)], e) of
                        (mem, B s) -> B s
                        _ -> error "La expresion es erronea para Not" 
evale (e@(Iszero e1)) = case evals ([(0, Void)], e) of
                        (mem, B s) -> B s
                        _ -> error "La expresion es erronea para Iszero" 
evale (e@(Lt e1 e2)) = case evals ([(0, Void)], e) of
                        (mem, B s) -> B s
                        _ -> error "La expresion es erronea para Lt"                     
evale (e@(Gt e1 e2)) = case evals ([(0, Void)], e) of
                        (mem, B s) -> B s
                        _ -> error "La expresion es erronea para Gt"                          
evale (e@(Eq e1 e2)) = case evals ([(0, Void)], e) of
                        (mem, B s) -> B s
                        _ -> error "La expresion es erronea para Eq"                         
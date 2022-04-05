module EAB where

data EAB = Var String 
        | Num Int
        | Bool Bool
        | Sum EAB EAB
        | Prod EAB EAB
        | Neg EAB
        | Pred EAB
        | Suc EAB
        | And EAB EAB 
        | Or EAB EAB
        | Not EAB
        | Iszero EAB
        | If EAB EAB EAB
        | Let EAB EAB  
        | Abs String EAB deriving (Show)
        
type Subst = (String, EAB)

fv :: EAB -> [String]
fv (Var s) = [s] --- V "x"  = ["x"]
fv (Num _) = []
fv (Bool b)= []
fv (Sum a b) = fv a ++ fv b
fv (Prod a b) = fv a ++ fv b
fv (Neg a) = fv a
fv (Pred a) = fv a
fv (Suc a) = fv a 
fv (And a b) = fv a ++ fv b
fv (Or a b) = fv a ++ fv b
fv (Iszero n) = fv n
fv (If a1 a2 a3) = fv a1 ++ fv a2 ++ fv a3
fv (Let a1 a2) = fv a1 ++ fv a2
fv (Abs x a1) = filter (/= x) (fv a1)

subs :: EAB -> Subst -> EAB
subs (Var s) (x, e) = if s == x
                     then e
                     else Var s
subs (Num n) _ = Num n
subs (And a1 a2) s = And (subs a1 s) (subs a2 s)
subs (Or a1 a2) s = Or (subs a1 s) (subs a2 s)
subs (Not a) s = Not (subs a s)
subs (Pred a) s = Pred (subs a s)
subs (Neg a) s = Neg (subs a s)
subs (Suc a) s = Suc (subs a s)
subs (Sum a1 a2) s = Sum (subs a1 s) (subs a2 s)
subs (Prod a1 a2) s = Prod (subs a1 s) (subs a2 s)
subs (Let a1 a2) s = Let (subs a1 s) (subs a2 s)
subs (Iszero a) s = Iszero (subs a s)
subs (If a1 a2 a3) s = If (subs a1 s) (subs a2 s) (subs a3 s)
subs (Abs z e) s@(x,r)
 | z==x || elem z (fv r) = error "error random"
 | otherwise = Abs z (subs e s)

eval1 :: EAB -> EAB
eval1 (Var x) = Var x
eval1 (Num n) = Num n
eval1 (Bool b)= Bool b
eval1 (Sum a b) = case (a, b) of
                (Num c, Num d) -> Num (c + d)
                (Num c, e) -> Sum (Num c) (eval1 e)
                (c, Num e) -> Sum (eval1 c) (Num e)
                (c, d) -> Sum (eval1 c) (eval1 d)
eval1 (Prod a b)= case (a, b) of
                (Num c, Num d) -> Num (c * d)
                (Num c, e) -> Prod (Num c) (eval1 e)
                (c, Num e) -> Prod (eval1 c) (Num e)
                (c, d) -> Prod (eval1 c) (eval1 d)
eval1 (Neg e) = case e of
                (Num c) -> (Num (-c))
                (c) -> Prod (c) (Neg (Num 1))
eval1 (Pred n) = case n of
                (Num c) -> Num (c - 1) 
                (c) -> Sum (c) (Neg (Num 1))
eval1 (Suc n) = case n of
                (Num c) -> Num (c + 1) 
                (c) -> Sum (c) (Num 1)
eval1 (Not a) = case a of
                (Bool True) -> Bool False
                (Bool False) -> Bool True
                (c) -> Not (eval1 a)  
eval1 (And a b) = case (a, b) of
                (Bool True, Bool True) -> Bool True
                (Bool False, Bool False) -> Bool False
                (Bool False, c) -> And (Bool False) (eval1 c)
                (c, Bool False) -> And (eval1 c) (Bool False)
                (Bool True, c) -> And (Bool True) (eval1 c)
                (c, Bool True) -> And (eval1 c) (Bool True)
                (c, d) -> And (eval1 c) (eval1 d)
eval1 (Or a b) = case (a, b) of
                (Bool True, Bool True) -> Bool True
                (Bool False, Bool False) -> Bool False
                (Bool True, c) -> Or (Bool True) (eval1 c)
                (c, Bool True) -> Or (eval1 c) (Bool True)
                (Bool False, c) -> Or (Bool False) (eval1 c)
                (c, Bool False) -> Or (eval1 c) (Bool False)
                (c, d) -> Or (eval1 c) (eval1 d)
eval1 (Iszero n) = case n of 
                  (Num c) -> if c == 0
                             then Bool True
                             else Bool False  
                  (c) -> Iszero (eval1(c))            
eval1 (If a b c) = case a of
                    (Bool True)  -> b    
                    (Bool False) -> c  
                    (d) -> If (eval1 d)(b)(c)
eval1 (Let e1 (Abs x e2))= case e1 of
                    (Num a) -> subs e2 (x, Num a)
                    (Bool b) -> subs e2 (x, Bool b)
                    (Var s) -> subs e2 (x, Var s)
                    (c) -> Let (eval1 c) (Abs (x) (e2))
                  

evals :: EAB -> EAB
evals (Var x) = Var x
evals (Num n) = Num n
evals (Bool b) = Bool b
evals (Sum a b) = case (a, b) of
                (Num c, Num d) -> eval1 (Sum (Num c) (Num d))
                (Num c, e) -> eval1 (Sum (Num c) (evals e))
                (c, Num e) -> eval1  (Sum (evals c) (Num e))
                (c, d) -> eval1 (Sum (evals c) (evals d))
evals (Prod a b)= case (a, b) of
                (Num c, Num d) -> eval1 (Prod (Num c) (Num d))
                (Num c, e) -> eval1 (Prod (Num c) (evals e))
                (c, Num e) -> eval1 (Prod (evals c) (Num e))
                (c, d) -> eval1 (Prod (evals c) (evals d))
evals (Neg e) = case e of
                --(Bool b) -> --quería que fuera como un not pero EAB no es Eq
                (Num c) -> (Num (-c))
                (c) -> eval1 (Prod (evals c) (eval1 (Neg (Num 1))) )
evals (Pred n) = case n of
                (Num c) -> eval1 (Num (c - 1)) 
                (c) -> eval1 (Sum (evals c) (eval1 (Neg(Num 1)))) 
evals (Suc n) = case n of
                (Num c) -> eval1 (Num (c +1)) 
                (c) -> eval1 (Sum (evals c) (eval1 (Num 1))) 
evals (Not a) = case a of
                (Bool True) -> eval1 (Bool False)
                (Bool False) -> eval1 (Bool True)
                (c) -> eval1 (Not (evals a))  
evals (And a b) = case (a, b) of
                (Bool True, Bool True) -> Bool True
                (Bool False, Bool False) -> Bool False
                (Bool False, c) -> eval1 (And (Bool False) (evals c))
                (c, Bool False) -> eval1 (And (evals c) (Bool False))
                (Bool True, c) -> eval1 (And (Bool True) (evals c))
                (c, Bool True) -> eval1 (And (evals c) (Bool True))
                (c, d) -> eval1 (And (evals c) (evals d))
evals (Or a b) = case (a, b) of
                (Bool True, Bool True) -> Bool True
                (Bool False, Bool False) -> Bool False
                (Bool True, c) -> eval1 (Or (Bool True) (evals c))
                (c, Bool True) -> eval1 (Or (evals c) (Bool True))
                (Bool False, c) -> eval1 (Or (Bool False) (evals c))
                (c, Bool False) -> eval1 (Or (evals c) (Bool False))
                (c, d) -> eval1 (Or (evals c) (evals d))
evals (Iszero n) = case n of 
                  (Num c) -> if c == 0
                             then Bool True
                             else Bool False  
                  (c) -> eval1 (Iszero (evals c))   
evals (If a b c) = case a of
                    (Bool True)  -> b    
                    (Bool False) -> c  
                    (d) -> eval1 (If (evals d)(eval1 b)(eval1 c))
evals (Let e1 (Abs x e2))= case e1 of
                    (Num a) -> eval1 (subs e2 (x, Num a))
                    (Bool b) -> subs e2 (x, Bool b)
                    (Var s) -> subs e2 (x, Var s)
                    (c) -> Let (eval1 c) (Abs (x) (e2))

eval :: EAB -> EAB
eval (Var x) = Var x
eval (Num n) = Num n
eval (Bool b) = Bool b
eval (Sum a b) = case evals(Sum a b) of 
                 (Num c) -> (Num c)
                 _ -> error "Se intento sumar algo distinto a un numero"
eval (Prod a b) = case evals(Prod a b) of 
                 (Num c) -> (Num c)
                 _ -> error "Se intento multiplicar algo distinto a un numero"                 
eval (Neg e) = case evals(Neg e) of 
                 (Num c) -> (Num c)
                 _ -> error "Se intento encontrar el negativo de algo distinto a un numero"
eval (Pred n) = case evals(Pred n) of 
                 (Num c) -> (Num c)
                 _ -> error "Se intento encontrar el predecesor de algo distinto a un numero"                 
eval (Suc n) = case evals(Suc n) of
                 (Num c) -> (Num c)
                 _ -> error "Se intento encontrar el sucesor de algo distinto a un numero"
eval (Not a) = case evals(Not a) of
                 (Bool c) -> (Bool c)
                 _ -> error "Se intento hacer Not a algo distinto de un Booleano"
eval (And a b) = case evals(And a b) of
                 (Bool c) -> (Bool c)
                 _ -> error "Se intento hacer And a algo distinto de un Booleano"                              
eval (Or a b) = case evals(Or a b) of 
                 (Bool c) -> (Bool c)
                 _ -> error "Se intento hacer Or a algo distinto de un Booleano"
eval (Iszero n) = case evals(Iszero n) of
                 (Bool c) -> (Bool c)
                 _ -> error "Se intento saber si algo distinto de un numero es cero"
eval (If a b c) = case evals(If a b c) of
                 (Bool c) -> (Bool c)
                 (Num c) -> (Num c)
                 (c) -> eval (c) 
eval (Let e1 (Abs x e2)) = case evals(Let e1 (Abs x e2)) of
                 (Bool c) -> (Bool c)
                 (Num c) -> (Num c)
                 (c) -> eval (c) 

data Type = TypeN -- Para Numeros
           |TypeB deriving (Show, Eq)  -- Para Booleanos

type Ctx = [(String,Type)] -- [("y", TypeN)] para contextos.


vt :: Ctx -> EAB -> Type -> Bool
vt [] (Var x) typ = False
vt _ (Num n) t = case t of 
                  TypeN -> True
                  _ -> False

vt _ (Bool b) t = case t of 
                  TypeB -> True
                  _ -> False

vt ((v, t):xs) e@(Var x) typ | t == typ && x == v = True
                             | otherwise = vt xs e typ

vt c (Sum e1 e2) t = case t of 
                      TypeN -> vt c e1 t && vt c e2 t
                      _ -> False

vt c (Prod e1 e2) t = case t of 
                      TypeN -> vt c e1 t && vt c e2 t
                      _ -> False

vt c (Neg e) t = case t of 
                  TypeN -> vt c e t
                  _ -> False

vt c (Pred e) t = case t of 
                  TypeN -> vt c e t
                  _ -> False    

vt c (Suc e) t = case t of 
                  TypeN -> vt c e t
                  _ -> False 

vt c (And e1 e2) t = case t of 
                      TypeB -> vt c e1 t && vt c e2 t
                      _ -> False

vt c (Or e1 e2) t = case t of 
                      TypeB -> vt c e1 t && vt c e2 t
                      _ -> False

vt c (Not e) t = case t of 
                  TypeB -> vt c e t
                  _ -> False 

vt c (Iszero e) t = case t of 
                  TypeB -> vt c e TypeN
                  _ -> False

vt c (If e1 e2 e3) t = case (vt c e1 TypeB) of 
                        True -> vt c e2 t && vt c e3 t
                        _ -> False

vt c (Let e1 (Abs x e2)) t = case (vt c e1 TypeB) of
                              True -> vt ((x, TypeB):c) e2 t
                              False -> vt ((x, TypeN):c) e2 t

--vt [] (Sum (Num 1) (Num 5)) TypeN  salida : True 
--vt [] (Sum (Num 10) (Bool False)) TypeN  salida : False

--evalt :: EAB -> EAB
--evalt _ = error "Implementar"

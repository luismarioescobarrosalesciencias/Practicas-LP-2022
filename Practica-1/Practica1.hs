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
fv (Neg _) = []
fv (Pred _) = []
fv (Suc _) = []
fv (And a b) = []
fv (Or a b) = []
fv (Iszero n) = []
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
                (Bool False, c) -> Bool False
                (c, Bool False) -> Bool False
                (Bool True, c) -> And (Bool True) (eval1 c)
                (c, Bool True) -> And (eval1 c) (Bool True)
                (c, d) -> And (eval1 c) (eval1 d)
eval1 (Or a b) = case (a, b) of
                (Bool True, Bool True) -> Bool True
                (Bool False, Bool False) -> Bool False
                (Bool True, c) -> Bool True
                (c, Bool True) -> Bool True
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
eval1 (Abs s e) = Abs (s) (eval1 e) 
                  

--evals :: EAB -> EAB



--eval :: EAB -> EAB
--eval _ = error "Implementar"

data Type = TypeN -- Para Numeros
           |TypeB  -- Para Booleanos
type Ctx = [(String,Type)] -- [("y", TypeN)] para contextos.

--vt :: Ctx -> EAB -> Type -> Bool
--vt _ _ _ = error "Implementar"

--evalt :: EAB -> EAB
--evalt _ = error "Implementar"
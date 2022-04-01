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
subs (Var s) (x,e) = if s == x
                     then e
                     else Var s
subs (Num n) _ = Num n
subs (Add a1 a2) s = Add (subs a1 s) (subs a2 s)
subs (Prod a1 a2) s = Prod (subs a1 s) (subs a2 s)
subs (Let a1 a2) s = Let (subs a1 s) (subs a2 s)
subs (Abs z e) s@(x,r)
 | z==x || elem z (fv r) = error "error random"
 | otherwise = Abs z (subs e s)

eval1 :: EAB -> EAB
eval1 (Var x) = error "Variable libre"
eval1 (Num n) = n
eval1 (Bool b)= b
eval1 (Sum a b) = eval1 a + eval1 b
eval1 (Prod a b)= eval1 a * eval1 b
eval (Neg e) = eval1 e * -1
eval1 (Pred n) = eval1 n -1
eval1 (Suc n) = eval1 n +1
eval1 (And a b) = eval1 a && eval1 b
eval1 (Or a b) = eval1 a || eval1 b
eval1 (Iszero n) = if eval1 n ==0
                   then true 
                   else false 
eval1 (If a b c) = -- ???
eval1 (Let e1 (Abs x e2))= eval1 (subs e2 (x, e1))
eval1 (Abs s e) = -- ???

evals :: EAB -> EAB
evals _ = error "Implementar"

eval :: EAB -> EAB
eval _ = error "Implementar"

data Type = () -- Definir los tipos de EAB
type Ctx = () -- Definir un sinomo para los contextos

vt :: Ctx -> EAB -> Type -> Bool
vt _ _ _ = error "Implementar"

evalt :: EAB -> EAB
evalt _ = error "Implementar"
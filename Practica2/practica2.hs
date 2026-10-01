type ID = String

data EAB = Num Int | Var ID | Bool Bool
  | Suma EAB EAB
  | Prod EAB EAB
  | Suc EAB
  | Pred EAB
  | Not EAB
  | If EAB EAB EAB
  | IsZero EAB
  | Lt EAB EAB | Gt EAB EAB | Eq EAB EAB
  | Let ID EAB EAB
  deriving (Eq)

type Env = [( ID , EAB ) ]

-- Introduccion
-- 1.
instance Show EAB where
  show (Num e) = show e
  show (Var id) = show id
  show (Bool b) = show b
  show (Suma e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Prod e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
  show (Suc e) = "suc(" ++ show e ++ ")"
  show (Pred e) = "pred(" ++ show e ++ ")"
  show (Not e) = "not(" ++ show e ++ ")"
  show (If b e1 e2) = "(If " ++ show b ++ " then " ++ show e1 ++ " else " ++ show e2 ++ ")"
  show (IsZero e) = "iszero(" ++ show e ++ ")"
  show (Lt e1 e2) = "(" ++ show e1 ++ "<" ++ show e2 ++ ")"
  show (Gt e1 e2) = "(" ++ show e1 ++ ">" ++ show e2 ++ ")"
  show (Eq e1 e2) = "(" ++ show e1 ++ "==" ++ show e2 ++ ")" 
  show (Let x e1 e2) = "(Let " ++ show x ++ " = " ++ show e1++ " in " ++ show e2 ++ ")"

-- 2. HECTOR 

-- 3.
sust :: ID -> EAB -> EAB -> EAB
sust _ _ (Num n) = Num n
sust z e1 (Var x) = if z == x
  then e1
  else Var x
sust _ _ (Bool b) = Bool b
sust id r (Suma x y) = Suma (sust id r x) (sust id r y)
sust id r (Prod x y) = Prod (sust id r x) (sust id r y)
sust id r (Suc e) = Suc (sust id r e)
sust id r (Pred e) = Pred (sust id r e)
sust id e1 (Not e2) = Not (sust id e1 e2)
sust id r (If b e1 e2) = If (sust id r b) (sust id r e1) (sust id r e2)
sust id r (IsZero e) = IsZero (sust id r e)
sust id r (Lt e1 e2) = Lt (sust id r e1) (sust id r e2)
sust id r (Gt e1 e2) = Gt (sust id r e1) (sust id r e2)
sust id r (Eq e1 e2) = Eq (sust id r e1) (sust id r e2)
sust id r (Let  s e1 e2) = if s == id
  then Let  s (sust id r e1) e2
  else if s `elem` fv r
          then error "Error: Captura de Variable libre"
          else Let s (sust id r e1) (sust id r e2)

-- Funcion auxiliar para sust
fv :: EAB -> [ID]
fv (Num _) = []
fv (Var x) = [x]
fv (Bool _) = []
fv (Suma x y) = (fv x) ++ (fv y)
fv (Prod x y) = (fv x) ++ (fv y)
fv (Suc e) = fv e
fv (Pred e) = fv e
fv (Not e) = fv e
fv (If b e1 e2) = (fv b) ++ (fv e1) ++ (fv e2)
fv (IsZero e) = fv e
fv (Lt e1 e2) = (fv e1) ++ (fv e2)
fv (Gt e1 e2) = (fv e1) ++ (fv e2)
fv (Eq e1 e2) = (fv e1) ++ (fv e2)
fv (Let x e1 e2) = (fv e1) ++ (filter (/= x) (fv e2))

-- Semantica Dinamica
-- 1.
evalStep :: EAB -> EAB
evalStep (Num n) = Num n
evalStep (Var x) = Var x
evalStep (Bool b) = Bool b
-- Suma 
evalStep (Suma (Num n) (Num m)) = Num (n + m)
evalStep (Suma (Num n) e2) = Suma (Num n) (evalStep e2)
evalStep (Suma e1 e2) = Suma (evalStep e1) e2
-- Producto 
evalStep (Prod (Num n) (Num m)) = Num (n * m)
evalStep (Prod (Num n) e2) = Prod (Num n) (evalStep e2)
evalStep (Prod e1 e2) = Prod (evalStep e1) e2
-- Sucesor
evalStep (Suc (Num n)) = Num (n+1)
evalStep (Suc e) = Suc (evalStep e)
-- Predecesor
evalStep (Pred (Num n)) = Num (n-1)
evalStep (Pred e) = Pred (evalStep e)
-- Not
evalStep (Not (Bool b)) = Bool (not b)
evalStep (Not e) = Not (evalStep e)
-- If
evalStep (If (Bool b) e1 e2) = if b
  then e1
  else e2
evalStep (If b e1 e2) = If (evalStep b) e1 e2
-- IsZero
evalStep (IsZero (Num n)) = Bool (n == 0)
evalStep (IsZero e) = IsZero (evalStep e)
-- Lt
evalStep (Lt (Num n) (Num m)) = Bool (n < m)
evalStep (Lt (Num n) e) = Lt (Num n) (evalStep e)
evalStep (Lt e1 e2) = Lt (evalStep e1) e2
-- Gt
evalStep (Gt (Num n) (Num m)) = Bool (n > m)
evalStep (Gt (Num n) e) = Gt (Num n) (evalStep e)
evalStep (Gt e1 e2) = Gt (evalStep e1) e2
-- Eq
evalStep (Eq (Num n) (Num m)) = Bool (n == m)
evalStep (Eq (Num n) e) = Eq (Num n) (evalStep e)
evalStep (Eq e1 e2) = Eq (evalStep e1) e2
-- Let
evalStep (Let x (Num n) e2) = sust x (Num n) e2
evalStep (Let x (Bool b) e2) = sust x (Bool b) e2
evalStep (Let x e1 e2) = Let x (evalStep e1) e2

-- 2.
evalDin :: EAB -> EAB
evalDin e = if e == (evalStep e)
  then e
  else evalDin (evalStep e)

-- 3. 
isValid :: EAB ->  Bool
isValid (Num n) = True
isValid (Bool e) = True
isValid e = if e == evalDin e
  then False
  else True
  

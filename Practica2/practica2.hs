{-
Práctica 2: 02/10/2026
-}

-- Sinónimo para nombre de las variables
type ID = String

-- Declaración de las Expresiones Aritméticas y Booleanas
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


-- Sinónimo para el ambiente (con variable y expresión ligada)
type Env = [( ID , EAB ) ]

-- Introducción


-- 1.
-- Para cada constructor se define como es su String
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

-- 2. 

-- Función auxiliar para dado un tipo Either desenvolverlo y regresar el primer constructor
primerE :: Either Int Bool -> Int 
primerE (Left a) = a 

-- Función auxiliar para dado un tipo Either desenvolverlo y regresar el segundo constructor
segundoE :: Either Int Bool -> Bool
segundoE (Right a) = a

-- Función para dado un ambiente y una varibale particular buscarla y regresar la expresión 
-- correspondiente, regresa error si esta no se encuentra
regresarEAB :: Env -> ID -> EAB
regresarEAB [] var = error ("No existe la variable " ++ var ++ " en el ambiente")
regresarEAB ((idx, expr):xs) var = if idx == var then expr else regresarEAB xs var

-- Función que para cada constructor define el valor que debe tomar dado el ambiente dado
evalEnv :: Env -> EAB -> Either Int Bool
evalEnv _ (Num x) =  Left x
evalEnv _ (Bool x) = Right x
evalEnv env (Var x) = evalEnv env (regresarEAB env x)
evalEnv env (Suma x y) = Left ( primerE(evalEnv env x) + primerE(evalEnv env y))
evalEnv env (Prod x y) = Left ( primerE(evalEnv env x) * primerE(evalEnv env y))
evalEnv env (Suc x) = Left ( primerE(evalEnv env x) + 1)
evalEnv env (Pred x) = Left ( primerE(evalEnv env x) - 1)
evalEnv env (Not x) = if segundoE(evalEnv env x) == True then Right False else Right True
evalEnv env (If x y z) = if segundoE(evalEnv env x) == True then evalEnv env y else evalEnv env z
evalEnv env (IsZero x) = if primerE(evalEnv env x) == 0 then Right True else Right False 
evalEnv env (Lt x y) = if primerE(evalEnv env x) < primerE(evalEnv env y) then Right True else Right False
evalEnv env (Gt x y) = if primerE(evalEnv env x) > primerE(evalEnv env y) then Right True else Right False
evalEnv env (Eq x y) = if (evalEnv env x) == (evalEnv env y) then Right True else Right False
evalEnv env (Let x y z) = evalEnv envModificado z
  where
    envModificado = [(x,y)] ++ env

-- 3.
-- Función que dada una variable, la expresión por la que se debe sustituir y la expresión en la 
-- que se debe sustituir, hace la sustitución si es posible (en particular para el caso del Let)
sust :: ID -> EAB -> EAB -> EAB
sust _ _ (Num n) = Num n
sust z e1 (Var x) = if z == x then e1 else Var x
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

-- Funcion auxiliar que dada una expresión, regresa una lista con sus variables libres
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
-- Función que calcula el paso inmediato siguiente de la expresión a evaluar
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
-- Función que calcula todos los pasos de una expresion hasta llegar a un valor o regresa
-- el estado bloqueado, es decir, acaba si hay dos pasos iguales consecutivos
evalDin :: EAB -> EAB
evalDin e = if e == (evalStep e)
  then e
  else evalDin (evalStep e)

-- 3. 
-- Función que verifica si una expresión es evaluable a otra expresión
isValid :: EAB ->  Bool
isValid (Num n) = True
isValid (Bool e) = True
isValid e = if e == evalDin e
  then False
  else True
  

-- Semántica Estática:

-- Sinónimo para definir el contexto de una expresión, es decir, la variable y el tipo
type Ctx = [(ID, Type)]

-- Tipo de dato con los tipos que estaremos manejando
data Type = Nat | Boolean deriving (Eq, Show)

-- Función auxiliar para regresar en tipo de una variable según el contexto dado, regresa
-- error si la variable no se encuentra en el contexto
revisarContexto :: ID -> Ctx -> Type
revisarContexto var [] = error ("Variable " ++ var ++ " No declarada en contexto")
revisarContexto id ((idx,tipo):xs) = if id == idx then tipo else revisarContexto id xs

-- Función que dada una expresión regresa el tipo final que tendrá si es correcta en sus tipos
-- o un error si halla algun error de tipos, indicando que se esperaba y lo que se tiene
typeEAB :: Ctx -> EAB -> Type
typeEAB _ (Num x) = Nat
typeEAB _ (Bool x) = Boolean
typeEAB ctx (Var id) = revisarContexto id ctx
-- Suma
typeEAB ctx (Suma x y)
  | (typeEAB ctx x) == Boolean = error ("Expected Nat: " ++ show x)
  | (typeEAB ctx y) == Boolean = error ("Expected Nat: " ++ show y)
  | otherwise = Nat
-- Prod
typeEAB ctx (Prod x y)
  | (typeEAB ctx x) == Boolean = error ("Expected Nat: " ++ show x)
  | (typeEAB ctx y) == Boolean = error ("Expected Nat: " ++ show y)
  | otherwise = Nat
-- Suc
typeEAB ctx (Suc x)
  | (typeEAB ctx x) == Boolean = error ("Expected Nat: " ++ show x)
  | otherwise = Nat
-- Pred
typeEAB ctx (Pred x)
  | (typeEAB ctx x) == Boolean = error ("Expected Nat: " ++ show x)
  | otherwise = Nat
-- Not
typeEAB ctx (Not x)
  | (typeEAB ctx x) == Nat = error ("Expected Boolean: " ++ show x)
  | otherwise = Boolean
-- If
typeEAB ctx (If x y z)
  | (typeEAB ctx x) == Nat = error ("Expected Boolean: " ++ show x)
  | (typeEAB ctx y) /= (typeEAB ctx z) = error ("Se tienen distintos tipos")
  | otherwise = typeEAB ctx y
-- isZero
typeEAB ctx (IsZero x)
  | (typeEAB ctx x) == Boolean = error ("Expected Nat: " ++ show x)
  | otherwise = Boolean
-- Lt
typeEAB ctx (Lt x y)
  | (typeEAB ctx x) == Boolean = error ("Expected Nat: " ++ show x)
  | (typeEAB ctx y) == Boolean = error ("Expected Nat: " ++ show y)
  | otherwise = Boolean
-- Gt
typeEAB ctx (Gt x y)
  | (typeEAB ctx x) == Boolean = error ("Expected Nat: " ++ show x)
  | (typeEAB ctx y) == Boolean = error ("Expected Nat: " ++ show y)
  | otherwise = Boolean
-- Eq
typeEAB ctx (Eq x y)
  | (typeEAB ctx x) /= (typeEAB ctx y) = error ("Se tienen distintos tipos")
  | otherwise = Boolean
-- Let
typeEAB ctx (Let x y z) = typeEAB ([(x, tipoY)] ++ ctx) z
  where
    tipoY = typeEAB ctx y

-- Función que dada una expresión la evalua a su valor final, pero antes haciendo la
-- verificación de tipos, regresa error si hay alguno
evalEst :: EAB -> Either Int Bool
evalEst expr 
  | (typeEAB [] expr) == Nat = evalEnv [] expr
  | (typeEAB [] expr) == Boolean = evalEnv [] expr
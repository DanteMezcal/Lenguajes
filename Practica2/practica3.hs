--Importamos para trabajar con set 
import Data.Set (Set)
import qualified Data.Set as Set

type ID = String

data LNLambda = Bvar Int | Fvar ID | AbsL LNLambda | AppL LNLambda LNLambda deriving Eq
  
--1. Introduccion
instance Show LNLambda where
  show (Bvar i) = show i
  show (Fvar id) = show id
  show (AbsL e) = "\\.(" ++ show e ++ ")"
  show (AppL e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"

--2. Manipulacion de Abstracciones
type SubstL = (Int, ID)

varOpen :: LNLambda -> SubstL -> LNLambda
varOpen (Bvar n) (i, id) = if n == i
  then Fvar id
  else Bvar n
varOpen (Fvar id) _ = Fvar id
varOpen (AppL e1 e2) x = AppL (varOpen e1 x) (varOpen e2 x)
varOpen (AbsL e) (i, id) = AbsL (varOpen e (i+1, id))

varClose :: LNLambda -> SubstL -> LNLambda
varClose (Bvar n) _ = Bvar n
varClose (Fvar id1) (i, id2)= if id1 == id2
  then Bvar i
  else Fvar id1
varClose (AppL e1 e2) x = AppL (varClose e1 x) (varClose e2 x)
varClose (AbsL e)  (i, id) = AbsL (varClose e (i+1, id))

--3. Terminos mal formados 
locClose :: LNLambda -> Bool
locClose (Bvar _) = False
locClose (Fvar _) = True
locClose (AppL e1 e2) = (locClose e1) && (locClose e2)
locClose (AbsL e) = locClose (varOpen e (0, "temp"))

locCloseK :: LNLambda -> Int -> Bool
locCloseK (Bvar i) k = i < k
locCloseK (Fvar _) _ = True
locCloseK (AppL e1 e2) k = (locCloseK e1 k) && (locCloseK e2 k)
locCloseK (AbsL e) k = locCloseK e (k+1)

--4. Analisis de terminos
freeVar :: LNLambda -> Set ID
freeVar (Bvar _) = Set.empty
freeVar (Fvar id) = Set.singleton id
freeVar (AppL e1 e2) = Set.union (freeVar e1) (freeVar e2)
freeVar (AbsL e) = freeVar e

fresh :: LNLambda -> ID -> Bool
fresh e id = Set.notMember id (freeVar e)

closed :: LNLambda -> Bool
closed e = Set.null (freeVar e)

--5. Sustitucion y evaluacion de terminos
type Subst = (ID, LNLambda)

subst :: LNLambda -> Subst -> LNLambda
subst (Bvar i) _ = Bvar i
subst (Fvar id1) (id2, e) = if id1 == id2
  then e
  else Fvar id1
subst (AppL e1 e2) s = AppL (subst e1 s) (subst e2 s)
subst (AbsL e) s = AbsL (subst e s)

betaR :: LNLambda -> LNLambda
betaR (Bvar i) = Bvar i
betaR (Fvar x) = Fvar x
betaR (AbsL e) = AbsL (betaR e)
betaR (AppL (AbsL e1) args) = betaR (subst (varOpen e1 (0, "temp")) ("temp", args))
betaR (AppL e1 e2) = AppL (betaR e1) (betaR e2)

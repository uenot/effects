module Pretnar.Lang where

import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as Set

data Value
  = Var String
  | BoolVal Bool
  | StringVal String
  | UnitVal
  | Func String ValueType Computation
  | Tuple Value Value
  | HandlerVal Handler
  deriving (Show)

-- name, continuation
data ReturnClause = ReturnClause String Computation deriving (Show)

-- parameter, continuation name, body
data OpClause = OpClause String String Computation deriving (Show)

-- type of return clause param (NOT OUTPUT!) [added from paper], optional return clause, operation clauses
data Handler = Handler ValueType (Maybe ReturnClause) [(String, OpClause)] deriving (Show)

data Computation
  = Return Value
  | -- id, parameter, result, continuation
    Perform String Value String Computation
  | Let String Computation Computation
  | If Value Computation Computation
  | Join Value Value String Computation
  | First Value String Computation
  | Second Value String Computation
  | App Value Value
  | With Value Computation
  deriving (Show)

substituteOps :: String -> Value -> [(String, OpClause)] -> [(String, OpClause)]
substituteOps _ _ [] = []
substituteOps x v ((n, OpClause y k c) : tl)
  | x == y || x == k = (n, OpClause y k c) : substituteOps x v tl
  | otherwise = (n, OpClause y k $ substituteComp x v c) : substituteOps x v tl

substituteValue :: String -> Value -> Value -> Value
substituteValue x v (Var x')
  | x == x' = v
  | otherwise = Var x'
substituteValue _ _ (BoolVal b) = BoolVal b
substituteValue _ _ (StringVal s) = StringVal s
substituteValue x v (Func y tp c)
  | x == y = Func y tp c
  | otherwise = Func y tp $ substituteComp x v c
substituteValue _ _ UnitVal = UnitVal
substituteValue x v (Tuple v1 v2) = Tuple (substituteValue x v v1) (substituteValue x v v2)
substituteValue x v (HandlerVal (Handler tp rc ops)) =
  let ops' = substituteOps x v ops
   in case rc of
        Nothing -> HandlerVal $ Handler tp rc ops'
        Just (ReturnClause y c) ->
          let c' = if x == y then c else substituteComp x v c
           in HandlerVal $ Handler tp (Just $ ReturnClause y c') ops'

substituteComp :: String -> Value -> Computation -> Computation
substituteComp x v (Return v') = Return $ substituteValue x v v'
substituteComp x v (Perform n v' y c)
  | x == y = Perform n (substituteValue x v v') y c
  | otherwise = Perform n (substituteValue x v v') y (substituteComp x v c)
substituteComp x v (Let y c1 c2)
  | x == y = Let y (substituteComp x v c1) c2
  | otherwise = Let y (substituteComp x v c1) (substituteComp x v c2)
substituteComp x v (If v' c1 c2) = If (substituteValue x v v') (substituteComp x v c1) (substituteComp x v c2)
substituteComp x v (Join s1 s2 y c)
  | x == y = Join (substituteValue x v s1) (substituteValue x v s2) y c
  | otherwise = Join (substituteValue x v s1) (substituteValue x v s2) y (substituteComp x v c)
substituteComp x v (App v1 v2) = App (substituteValue x v v1) (substituteValue x v v2)
substituteComp x v (First v' y c)
  | x == y = First (substituteValue x v v') y c
  | otherwise = First (substituteValue x v v') y (substituteComp x v c)
substituteComp x v (Second v' y c)
  | x == y = Second (substituteValue x v v') y c
  | otherwise = Second (substituteValue x v v') y (substituteComp x v c)
substituteComp x v (With v' c) = With (substituteValue x v v') (substituteComp x v c)

step :: Computation -> Maybe Computation
step (Let x (Return v) c2) = return $ substituteComp x v c2
step (Let x (Perform n v y c1) c2) = return $ Perform n v y $ Let x c1 c2
step (Let x c1 c2) = do
  c1' <- step c1
  return $ Let x c1' c2
step (If (BoolVal True) c1 _) = return c1
step (If (BoolVal False) _ c2) = return c2
step (Join (StringVal s1) (StringVal s2) y c) = return $ substituteComp y (StringVal $ s1 ++ " " ++ s2) c
step (First (Tuple v1 _) y c) = return $ substituteComp y v1 c
step (Second (Tuple _ v2) y c) = return $ substituteComp y v2 c
step (App (Func x _ c) v) = return $ substituteComp x v c
step (With (HandlerVal (Handler _ rc _)) (Return v)) = case rc of
  Just (ReturnClause x c) -> return $ substituteComp x v c
  Nothing -> return $ Return v
step (With h (Perform n v y c)) = case h of
  (HandlerVal (Handler _ _ ops)) -> case lookup n ops of
    Just (OpClause x k c') -> return $ substituteComp x v $ substituteComp k (Func y UnitType $ With h c) c'
    Nothing -> return $ Perform n v y $ With h c
  _ -> Nothing
step (With h c) = do
  c' <- step c
  return $ With h c'
step _ = Nothing

multistep :: Computation -> Computation
multistep c = maybe c multistep (step c)

data ValueType
  = BoolType
  | UnitType
  | StringType
  | VoidType
  | TupleType ValueType ValueType
  | FuncType ValueType CompType
  | HandlerType CompType CompType
  deriving (Show, Eq)

data CompType = CompType ValueType (Set String) deriving (Show, Eq)

type TypeContext = [(String, ValueType)]

data OpType = OpType ValueType ValueType deriving (Show)

type OpSignature = [(String, OpType)]

diff :: CompType -> CompType -> Maybe (Set String)
diff (CompType v1 ops1) (CompType v2 ops2) = do
  guard $ v1 == v2
  guard $ ops1 `Set.isSubsetOf` ops2
  return $ ops2 `Set.difference` ops1

typecheckValue :: OpSignature -> TypeContext -> Value -> Maybe ValueType
typecheckValue _ ctx (Var x) = lookup x ctx
typecheckValue _ _ (BoolVal _) = return BoolType
typecheckValue _ _ (StringVal _) = return StringType
typecheckValue _ _ UnitVal = return UnitType
typecheckValue sig ctx (Tuple v1 v2) = do
  t1 <- typecheckValue sig ctx v1
  t2 <- typecheckValue sig ctx v2
  return $ TupleType t1 t2
typecheckValue sig ctx (Func x tx c) = FuncType tx `fmap` typecheckComp sig ((x, tx) : ctx) c
typecheckValue sig ctx (HandlerVal (Handler ta rc ops)) =
  do
    (CompType tr opsr) <- checkReturn ta rc
    opsDiff <- checkOps (CompType tr opsr) ops
    return $ HandlerType (CompType ta (Set.fromList $ map fst ops)) (CompType tr (Set.union opsr opsDiff))
  where
    checkReturn :: ValueType -> Maybe ReturnClause -> Maybe CompType
    checkReturn tp Nothing = return $ CompType tp Set.empty
    checkReturn _ (Just (ReturnClause x c)) = typecheckComp sig ((x, ta) : ctx) c

    checkOps :: CompType -> [(String, OpClause)] -> Maybe (Set String)
    checkOps _ [] = return Set.empty
    checkOps tb ((n, OpClause x k c) : tl) = case lookup n sig of
      Nothing -> Nothing
      Just (OpType a b) -> do
        tb' <- typecheckComp sig ((x, a) : (k, FuncType b tb) : ctx) c
        opsDiff <- diff tb tb'
        res <- checkOps tb tl
        return $ opsDiff `Set.union` res

typecheckComp :: OpSignature -> TypeContext -> Computation -> Maybe CompType
typecheckComp sig ctx (Return x) = do
  tx <- typecheckValue sig ctx x
  return $ CompType tx Set.empty
typecheckComp sig ctx (Perform n v y c) = case lookup n sig of
  Nothing -> Nothing
  Just (OpType a b) -> do
    tv <- typecheckValue sig ctx v
    if tv /= a
      then Nothing
      else do
        CompType tcv ops <- typecheckComp sig ((y, b) : ctx) c
        return $ CompType tcv (Set.insert n ops)
typecheckComp sig ctx (Let x c1 c2) = do
  CompType tx ops1 <- typecheckComp sig ctx c1
  CompType t2 ops2 <- typecheckComp sig ((x, tx) : ctx) c2
  return $ CompType t2 $ ops1 `Set.union` ops2
typecheckComp sig ctx (App v1 v2) = do
  t1 <- typecheckValue sig ctx v1
  t2 <- typecheckValue sig ctx v2
  case t1 of
    FuncType tx tc -> if tx == t2 then return tc else Nothing
    _ -> Nothing
typecheckComp sig ctx (If b t f) = do
  tb <- typecheckValue sig ctx b
  tt <- typecheckComp sig ctx t
  tf <- typecheckComp sig ctx f
  if tb == BoolType && tt == tf then return tt else Nothing
typecheckComp sig ctx (Join s1 s2 y c) = do
  t1 <- typecheckValue sig ctx s1
  t2 <- typecheckValue sig ctx s2
  if t1 == StringType && t2 == StringType
    then
      typecheckComp sig ((y, StringType) : ctx) c
    else
      Nothing
typecheckComp sig ctx (First v y c) = do
  tv <- typecheckValue sig ctx v
  case tv of
    TupleType t1 _ -> typecheckComp sig ((y, t1) : ctx) c
    _ -> Nothing
typecheckComp sig ctx (Second v y c) = do
  tv <- typecheckValue sig ctx v
  case tv of
    TupleType _ t2 -> typecheckComp sig ((y, t2) : ctx) c
    _ -> Nothing
typecheckComp sig ctx (With v c) = do
  tv <- typecheckValue sig ctx v
  tc <- typecheckComp sig ctx c
  case tv of
    HandlerType tin (CompType vout opsOut) -> do
      opsDiff <- diff tin tc
      return $ CompType vout $ Set.union opsOut opsDiff
    _ -> Nothing
module HandlerCalculus.Basic where

import Debug.Trace (trace)

data Kind = ValueKind | CompKind | EffectKind
  deriving (Show)

data Type
  = ZeroType
  | HandlerType Type Type
  | CompType Type Type
  | EffectType [(String, ([Type], Type))]

instance Show Type where
  show ZeroType = "0"
  show (HandlerType (CompType (EffectType []) ZeroType) (CompType (EffectType []) ZeroType)) = "1"
  show (HandlerType c d)
    | show c == "1" = "{" ++ show d ++ "}"
    | otherwise = parenthesizeShort c ++ " => " ++ parenthesizeShort d
    where
      parenthesizeShort :: (Show a) => a -> String
      parenthesizeShort x
        | length (show x) < 30 = show x
        | otherwise = "(" ++ show x ++ ")"
  show (CompType (EffectType []) a) = show a
  show (CompType e a) = show e ++ show a
  show (EffectType ls) = "[" ++ showEffectType ls
    where
      showEffectType :: [(String, ([Type], Type))] -> String
      showEffectType [] = "]"
      showEffectType [(op, (params, ret))] = op ++ ": (" ++ listParams params ++ ") -> " ++ show ret ++ "]"
      showEffectType ((op, (params, ret)) : tl) = op ++ ": (" ++ listParams params ++ ") -> " ++ show ret ++ "; " ++ show tl

      listParams :: [Type] -> String
      listParams [] = ""
      listParams [hd] = show hd
      listParams (hd : tl) = show hd ++ ", " ++ listParams tl

data Term
  = Var String
  | Handler (String, Term) [(String, ([String], String, Term))]
  | Absurd Term
  | Return Term
  | Let String Type Term Term
  | Perform String [Term]
  | With Term Term Type
  deriving (Show)

kindcheck :: Type -> Maybe Kind
kindcheck ZeroType = return ValueKind
kindcheck (HandlerType c d) = do
  kc <- kindcheck c
  kd <- kindcheck d
  case (kc, kd) of
    (CompKind, CompKind) -> return ValueKind
    _ -> Nothing
kindcheck (EffectType ops) = kindcheckEffect $ map snd ops
  where
    kindcheckEffect :: [([Type], Type)] -> Maybe Kind
    kindcheckEffect [] = return EffectKind
    kindcheckEffect ((params, ret) : tl) = do
      mapM_ verifyIsValue params
      verifyIsValue ret
      kindcheckEffect tl
    verifyIsValue :: Type -> Maybe ()
    verifyIsValue x = do
      kx <- kindcheck x
      case kx of
        ValueKind -> return ()
        _ -> Nothing
kindcheck (CompType e v) = do
  ke <- kindcheck e
  kv <- kindcheck v
  case (ke, kv) of
    (EffectKind, ValueKind) -> return CompKind
    _ -> Nothing

type TypeContext = [(String, Type)]

type EffectContext = [(String, ([Type], Type))]

type Context = (TypeContext, EffectContext)

addToTC :: Context -> String -> Type -> Context
addToTC (tpctx, efctx) x tx = ((x, tx) : tpctx, efctx)

getEC :: Context -> Type
getEC = EffectType . snd

addToEC :: Context -> [(String, ([Type], Type))] -> Context
addToEC (tpctx, efctx) e = (tpctx, e ++ efctx)

lookupEC :: Context -> String -> Maybe ([Type], Type)
lookupEC ctx name = lookup name (snd ctx)

unwrapComp :: Type -> Maybe ([(String, ([Type], Type))], Type)
unwrapComp t = case t of
  CompType (EffectType effects) value -> return (effects, value)
  _ -> Nothing

typesEqual :: Type -> Type -> Maybe ()
typesEqual ZeroType ZeroType = return ()
typesEqual (HandlerType a b) (HandlerType c d) = typesEqual a c >> typesEqual b d
typesEqual (CompType e a) (CompType f b) = typesEqual e f >> typesEqual a b
typesEqual (EffectType []) (EffectType []) = return ()
typesEqual (EffectType (ahd : atl)) (EffectType bls) = do
  let (op, (aParamTypes, aRetType)) = ahd
  (bParamTypes, bRetType) <- lookup op bls
  mapM_ (uncurry typesEqual) (zip aParamTypes bParamTypes)
  typesEqual aRetType bRetType
  typesEqual (EffectType atl) (EffectType $ filter ((/= op) . fst) bls)
typesEqual _ _ = Nothing

typecheck :: TypeContext -> Term -> Type -> Maybe ()
typecheck ctx (Var x) expectedType = do
  tx <- lookup x ctx
  typesEqual tx expectedType
typecheck ctx (Absurd x) _ = typecheck ctx x ZeroType
typecheck ctx (Return x) expectedType = do
  (_, expectedValueType) <- unwrapComp expectedType
  typecheck ctx x expectedValueType
typecheck ctx (Let x tx c1 c2) expectedType = do
  (e, _) <- unwrapComp expectedType
  typecheck ctx c1 (CompType (EffectType e) tx)
  typecheck ((x, tx) : ctx) c2 expectedType
typecheck ctx (Perform op args) expectedType = do
  (e, b) <- unwrapComp expectedType
  (expectedArgTypes, retType) <- lookup op e
  typesEqual b retType
  mapM_ (uncurry $ typecheck ctx) $ zip args expectedArgTypes
typecheck ctx (With h m tm) expectedType = do
  typecheck ctx m tm
  typecheck ctx h (HandlerType tm expectedType)
typecheck ctx (Handler retClause opClauses) (HandlerType c d) = do
  (tcEffects, tcReturn) <- unwrapComp c
  let (retParam, retBody) = retClause
  typecheck ((retParam, tcReturn) : ctx) retBody d
  mapM_ (checkOps tcEffects d) opClauses
  where
    checkOps :: [(String, ([Type], Type))] -> Type -> (String, ([String], String, Term)) -> Maybe ()
    checkOps effs d (op, (args, r, body)) = do
      (argTypes, retType) <- lookup op effs
      let contType = HandlerType retType d
      let ctx' = zip args argTypes ++ (r, contType) : ctx
      typecheck ctx' body d
typecheck _ (Handler _ _) _ = Nothing
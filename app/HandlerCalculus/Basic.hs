{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module HandlerCalculus.Basic where

data Kind = ValueKind | CompKind | EffectKind
  deriving (Show)

data Type
  = ZeroType
  | HandlerType Type Type
  | CompType Type Type
  | EffectType [(String, ([Type], Type))]

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

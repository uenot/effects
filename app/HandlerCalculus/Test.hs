module HandlerCalculus.Test where

import HandlerCalculus.Basic

test :: IO ()
test = do
  let compify = CompType (EffectType [])
  let zeroComp = compify ZeroType

  let unitType = HandlerType zeroComp zeroComp
  let unit = Handler ("x", Return $ Var "x") []

  let thunkifyType = HandlerType (compify unitType)
  let thunkifyTerm t = Handler ("_", t) []

  let unitPairType = thunkifyType $ CompType (EffectType [("pair", ([unitType, unitType], ZeroType))]) ZeroType
  let unitPair = thunkifyTerm $ Perform "pair" [unit, unit]

  let bot = Handler ("x", Absurd $ Var "x")
  let unthunk v = With v (Return unit) (compify unitType)
  let unthunkedUnitPairType = CompType (EffectType [("pair", ([unitType, unitType], ZeroType))]) ZeroType

  let first = With (bot [("pair", (["x1", "x2"], "_", Return $ Var "x1"))]) (unthunk unitPair) unthunkedUnitPairType

  test_ first (compify unitType)

test_ :: Term -> Type -> IO ()
test_ term tp = do
  putStrLn $ "Term: " ++ show term
  putStrLn $ "Expected type: " ++ show tp
  case typecheck [] term tp of
    Just () -> do
      putStrLn "TYPECHECKING PASSED"
      putStrLn $ "Kind: " ++ show (kindcheck tp)
    _ -> putStrLn "TYPECHECKING FAILED"
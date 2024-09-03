module Pretnar.Example where

import Pretnar.Lang

testSig :: OpSignature
testSig =
  [ ("read", OpType UnitType StringType),
    ("print", OpType StringType UnitType),
    ("raise", OpType StringType VoidType),
    ("decide", OpType UnitType BoolType)
  ]

metaHandler :: Computation -> IO (Maybe Computation)
metaHandler (Perform "read" _ y c) = do
  str <- getLine
  return $ Just $ substituteComp y (StringVal str) c
metaHandler (Perform "print" v y c) = case v of
  (StringVal str) -> do
    putStrLn str
    return $ Just $ substituteComp y UnitVal c
  _ -> return Nothing
metaHandler _ = return Nothing

eval :: Computation -> IO Computation
eval c = case step c of
  Just c' -> eval c'
  Nothing -> do
    c' <- metaHandler c
    case c' of
      Just c'' -> eval c''
      Nothing -> return c

printFullName :: Computation
printFullName =
  Perform "print" (StringVal "What is your forename?") "_" $
    Perform "read" UnitVal "forename" $
      Perform "print" (StringVal "What is your surname?") "_" $
        Perform "read" UnitVal "surname" $
          Join (Var "forename") (Var "surname") "res" $
            Perform "print" (Var "res") "_" $
              Return UnitVal

alwaysRead :: Value
alwaysRead =
  Func "s" StringType $
    Return $
      HandlerVal $
        Handler UnitType Nothing [("read", OpClause "_" "k" $ App (Var "k") (Var "s"))]

testAlwaysRead :: Computation
testAlwaysRead = Let "readBob" (App alwaysRead $ StringVal "Bob") $ With (Var "readBob") printFullName

reverseHandler :: Value
reverseHandler =
  HandlerVal $
    Handler
      UnitType
      Nothing
      [ ( "print",
          OpClause "s" "k" $
            Let "_" (App (Var "k") UnitVal) $
              Perform "print" (Var "s") "res" $
                Return $
                  Var "res"
        )
      ]

abc :: Computation
abc =
  Let "_" (Perform "print" (StringVal "A") "x" $ Return $ Var "x") $
    Let "_" (Perform "print" (StringVal "B") "x" $ Return $ Var "x") $
      Let "_" (Perform "print" (StringVal "C") "x" $ Return $ Var "x") $
        Return UnitVal

testReverse :: Computation
testReverse = With reverseHandler abc

collect :: Value
collect =
  HandlerVal $
    Handler
      UnitType
      (Just $ ReturnClause "x" $ Return $ Tuple (Var "x") (StringVal ""))
      [ ( "print",
          OpClause "s" "k" $
            Let "pair" (App (Var "k") UnitVal) $
              First (Var "pair") "x" $
                Second (Var "pair") "acc" $
                  Join (Var "s") (Var "acc") "res" $
                    Return $
                      Tuple (Var "x") (Var "res")
        )
      ]

testCollect :: Computation
testCollect = With collect abc

testCollectReverse :: Computation
testCollectReverse = With collect testReverse

collectPP :: Value
collectPP =
  HandlerVal $
    Handler
      UnitType
      ( Just $
          ReturnClause "x" $
            Return $
              Func "acc" StringType $
                Return $
                  Tuple (Var "x") (Var "acc")
      )
      [ ( "print",
          OpClause "s" "k" $
            Return $
              Func "acc" StringType $
                Join (Var "acc") (Var "s") "res" $
                  Let "f" (App (Var "k") UnitVal) $
                    App (Var "f") (Var "res")
        )
      ]

testCollectPP :: Computation
testCollectPP = Let "f" (With collectPP abc) $ App (Var "f") $ StringVal ""

handleDefault :: Value
handleDefault =
  Func "s" StringType $
    Return $
      HandlerVal $
        Handler StringType Nothing [("raise", OpClause "_" "_" $ Return $ Var "s")]

testDefault :: Computation
testDefault =
  Let "handler" (App handleDefault $ StringVal "default") $
    With (Var "handler") $
      Let "x" (Return $ StringVal "test") $
        Perform "raise" (StringVal "test error") "_" $
          Return $
            Var "x"

choose :: Value
choose =
  Func "xy" (TupleType StringType StringType) $
    Perform "decide" UnitVal "b" $
      If
        (Var "b")
        (First (Var "xy") "x" $ Return $ Var "x")
        (Second (Var "xy") "y" $ Return $ Var "y")

chooseTest :: Computation
chooseTest =
  Let "x1" (App choose (Tuple (StringVal "a") (StringVal "b"))) $
    Let "x2" (App choose (Tuple (StringVal "c") (StringVal "d"))) $
      Join (Var "x1") (Var "x2") "res" $
        Return $
          Var "res"

pickTrue :: Value
pickTrue =
  HandlerVal $
    Handler StringType Nothing $
      [("decide", OpClause "_" "k" $ Let "res" (App (Var "k") (BoolVal True)) $ Return $ Var "res")]

testPickTrue :: Computation
testPickTrue = With pickTrue chooseTest

test :: Computation -> IO ()
test program = do
  let tp = typecheckComp testSig [] program
  putStrLn $ "TYPE: " ++ show tp
  putStrLn "Evaluating..."
  res <- eval program
  putStrLn $ "RESULT: " ++ show res

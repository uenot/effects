module Pretnar.Example where

import Pretnar.Lang

testProgram :: Computation
testProgram =
  Op "read" Unit "str" $
    Op "print" (Var "str") "res" $
      Return $
        Var "res"

testSig :: OpSignature
testSig =
  [ ("read", OpType UnitType StringType),
    ("print", OpType StringType UnitType),
    ("raise", OpType StringType VoidType),
    ("decide", OpType UnitType BoolType)
  ]

metaHandler :: Computation -> IO (Maybe Computation)
metaHandler (Op "read" _ y c) = do
  str <- getLine
  return $ Just $ substituteComp y (StringVal str) c
metaHandler (Op "print" v y c) = case v of
  (StringVal str) -> do
    putStrLn str
    return $ Just $ substituteComp y Unit c
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
  Op "print" (StringVal "What is your forename?") "_" $
    Op "read" Unit "forename" $
      Op "print" (StringVal "What is your surname?") "_" $
        Op "read" Unit "surname" $
          Join (Var "forename") (Var "surname") "res" $
            Op "print" (Var "res") "_" $
              Return Unit

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
            Let "_" (App (Var "k") Unit) $
              Op "print" (Var "s") "res" $
                Return $
                  Var "res"
        )
      ]

abc :: Computation
abc =
  Let "_" (Op "print" (StringVal "A") "x" $ Return $ Var "x") $
    Let "_" (Op "print" (StringVal "B") "x" $ Return $ Var "x") $
      Let "_" (Op "print" (StringVal "C") "x" $ Return $ Var "x") $
        Return Unit

testReverse :: Computation
testReverse = With reverseHandler abc

collect :: Value
collect =
  HandlerVal $
    Handler
      UnitType
      (Just $ ReturnClause "x" $ Return $ Pair (Var "x") (StringVal ""))
      [ ( "print",
          OpClause "s" "k" $
            Let "pair" (App (Var "k") Unit) $
              First (Var "pair") "x" $
                Second (Var "pair") "acc" $
                  Join (Var "s") (Var "acc") "res" $
                    Return $
                      Pair (Var "x") (Var "res")
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
                  Pair (Var "x") (Var "acc")
      )
      [ ( "print",
          OpClause "s" "k" $
            Return $
              Func "acc" StringType $
                Join (Var "acc") (Var "s") "res" $
                  Let "f" (App (Var "k") Unit) $
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
        Op "raise" (StringVal "test error") "_" $
          Return $
            Var "x"

choose :: Value
choose =
  Func "xy" (TupleType StringType StringType) $
    Op "decide" Unit "b" $
      If
        (Var "b")
        (First (Var "xy") "x" $ Return $ Var "x")
        (Second (Var "xy") "y" $ Return $ Var "y")

chooseTest :: Computation
chooseTest =
  Let "x1" (App choose (Pair (StringVal "a") (StringVal "b"))) $
    Let "x2" (App choose (Pair (StringVal "c") (StringVal "d"))) $
      Join (Var "x1") (Var "x2") "res" $
        Return $
          Var "res"

pickTrue :: Value
pickTrue =
  HandlerVal $
    Handler StringType Nothing $
      [("decide", OpClause "_" "k" $ Let "res" (App (Var "k") TrueVal) $ Return $ Var "res")]

testPickTrue :: Computation
testPickTrue = With pickTrue chooseTest
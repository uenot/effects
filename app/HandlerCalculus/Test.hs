module HandlerCalculus.Test where

import HandlerCalculus.Basic

test :: IO ()
test = do
  let zeroComp = CompType (EffectType []) ZeroType
  let tp = HandlerType zeroComp zeroComp
  let k = kindcheck tp
  putStrLn $ show k
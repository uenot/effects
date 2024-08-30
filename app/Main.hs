module Main where

import Pretnar.Example
import Pretnar.Lang

main :: IO ()
main = do
  let program = testReverse
  let tp = typecheckComp testSig [] program
  putStrLn $ "TYPE: " ++ show tp
  putStrLn "Evaluating..."
  res <- eval program
  putStrLn $ "RESULT: " ++ show res

module Spec
    ( testgcd'
    ) where

import GCDRefactorum




testgcd' :: IO ()
testgcd' = do
  let d = gcd' 36 28 :: Integer
  putStrLn $ "gcd' 36 28 = " ++ (show d) ++ "."
  let e = gcd' 77 847 :: Integer
  putStrLn $ "gcd' 77 847 = " ++ (show e) ++ "."




main :: IO ()
main = do
  putStrLn "Test suite implemented shittily."
  testgcd'
  putStrLn "done-yet."
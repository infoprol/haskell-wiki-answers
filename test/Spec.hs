module Spec where

import Lib




testgcd' :: IO ()
testgcd' = do
  let d = gcd' 36 28 :: Integer
  putStrLn $ "gcd' 36 28 = " ++ (show d :: Integer)




main :: IO ()
main = do
  putStrLn "Test suite implemented shittily."
  testgcd'
  putStrLn "done-yet."
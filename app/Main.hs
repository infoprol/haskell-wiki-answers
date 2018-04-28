module Main where

import GCDRefactorum
import IORThey


testgcd' :: IO ()
testgcd' = do
  let d = gcd' 36 28 :: Integer
  putStrLn $ "gcd' 36 28 = " ++ (show d) ++ "."
  let e = gcd' 77 847 :: Integer
  putStrLn $ "gcd' 77 847 = " ++ (show e) ++ "."





main :: IO ()
main = doPoetry


xxmain :: IO ()
xxmain = do
  putStrLn $ "gcd'(36,24) = " ++ show (gcd' 36 24) 

--main = someFunc

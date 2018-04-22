module Main where
import Lib

main :: IO ()
main = do
  putStrLn $ "gcd'(36,24) = " ++ show (gcd' 36 24) 

--main = someFunc

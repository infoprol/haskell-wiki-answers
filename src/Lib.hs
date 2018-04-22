module Lib
    ( someFunc
    , gcd'
    , gcd''
    ) where

someFunc :: IO ()
someFunc = putStrLn "suck this forever!!"



-- off by fucking 1
gcd' :: Integer -> Integer -> Integer
gcd' m 0 = m
gcd' 0 _ = 0
gcd' m n
  | m < 0             = gcd' (abs m) n
  | n < 0             = gcd' m (abs n)
  | m < n             = gcd' n m
  | m == n            = n
  | otherwise         = gcd' (m `div` n) (m `mod` n)

gcd'' :: Integer -> Integer -> Integer
gcd'' m n = 0

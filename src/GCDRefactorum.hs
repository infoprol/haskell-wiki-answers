module GCDRefactorum
    ( someFunc
    , gcd'
    , gcd''
    , gcdTry
    ) where

import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "suck this forever!!"


{--
  special cases / terminating conditions all flim-flammed together-like.
--}
gcd' :: Integer -> Integer -> Integer
gcd' m 0 = m
gcd' 0 _ = 0
gcd' m n
    | m < 0             = gcd' (abs m) n
    | n < 0             = gcd' m (abs n)
    | m < n             = gcd' n m
    | m == n            = n
    | otherwise         = gcd' (m `div` n) (m `mod` n)





data Try a = Err String | Return a

instance Functor Try where
    fmap _ (Err e)      = Err e
    fmap f (Return x)   = Return (f x)


instance Applicative Try where
    pure x                  = Return x
    _ <*> Err e             = Err e
    Return fm <*> Return a  = fmap fm (Return a)


instance Monad Try where
    return x    = Return x
    fail msg    = Err msg

    Err e     >>= _     = Err e
    Return a  >>= f     = f a
 


gcd'' :: Integer -> Integer -> Integer
gcd'' m 0 = m
gcd'' m n = gcd'' (m `div` n) (m `mod` n)


gcdTry :: Integer -> Integer -> Try Integer
gcdTry m n
    | or [ m <= 0, n <= 0 ]     = Err "positive numbers only, goddammit!"
    | m < n                     = Err "arguments must be fed in non-increasing order, goddammit!"
    | otherwise                 = Return (gcd'' m n)


module GCDRefactorum
    ( someFunc
    , gcd'
    , gcd''
    ) where

--import Control.Monad
--import Data.List

someFunc :: IO ()
someFunc = putStrLn "suck this forever!!"


{--
  special cases / terminating conditions all flim-flammed together-like.
  also, doesn't work right.
--}
{--
xxgcd' :: Integereger -> Integereger -> Integereger
xxgcd' m 0 = m
xxgcd' 0 _ = 0
xxgcd' m n
    | m < 0             = xxgcd' (abs m) n
    | n < 0             = xxgcd' m (abs n)
    | m < n             = xxgcd' n m
    | m == n            = n
    | otherwise         = xxgcd' (m `div` n) (m `mod` n)
--}


-- might not work right all/most of time either,
-- at least it's shorter...
gcd' :: Integer -> Integer -> Integer
gcd' a b    | a < 0         = gcd' (abs a) b
            | b < 0         = gcd' a (abs b)
            | b == 0        = a
            | a < b         = gcd' b a
            | otherwise     = gcd' (a `div` b) (a `rem` b)






{---------------------------------------

    gcd'' - second go, now with lists and shit!

--}
(//) :: Integer -> Integer -> Integer
(//) x y = x `div` y

(%%) :: Integer -> Integer -> Integer
(%%) x y = x `mod` y

 
euclidsElems :: Integer -> Integer -> [ (Integer, Integer, Integer) ]
euclidsElems a b = (b, q, r) : euclidsElems q r
    where
        q = a // b
        r = a %% b

isAnswer :: (Integer, Integer, Integer) -> Bool
isAnswer (_, _, r)    | r == 0    = True
                    | otherwise = False

pluckAns :: (Integer, Integer, Integer) -> Integer
pluckAns eucliment = x where (x, _, _) = eucliment


gcd'' :: Integer -> Integer -> Integer
gcd'' a b = pluckAns $ (filter isAnswer (euclidsElems a b)) !! 0
        



















    
    
    
    
    
{---
    where
        ans _ _ = get$ takeWhile reRemains (euclidsElements a b)
        reRemains 
        euclidsElements x y = (y (x `div` y) (x `mod` y)) (:) 


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
 


gcd'' :: Integereger -> Integereger -> Integereger
gcd'' m 0 = m
gcd'' m n = gcd'' (m `div` n) (m `mod` n)


gcdTry :: Integereger -> Integereger -> Try Integereger
gcdTry m n
    | or [ m <= 0, n <= 0 ]     = Err "positive numbers only, goddammit!"
    | m < n                     = Err "arguments must be fed in non-increasing order, goddammit!"
    | otherwise                 = Return (gcd'' m n)
    
    
--}


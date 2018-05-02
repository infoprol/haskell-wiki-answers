module NNQs where

import System.IO


{-- https://wiki.haskell.org/99_questions/1_to_10 --}
{--
    NOTE: i'm leaving the functions with incomplete defs
    for now.  if nothing else, they could be wrapped in Maybe's
--}




--1
myLast :: [a] -> a  
myLast [x]  = x
myLast (x:xs) = myLast xs


--2
myButLast :: [a] -> a
myButLast (x:y:[])  = x
myButLast (x:xs)    = myButLast xs


--3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 0 = x
elementAt (x:xs) k = elementAt xs (k - 1)


--4
myLength :: [a] -> Int
myLength = foldl ((\x y -> x) . (+1)) 0
--myLength (x:xs) = myLength


--5
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []


--6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (myReverse xs)


-- 7
data NestedList a = Elem a | List [NestedList a] deriving Show

simpleFlatten :: [[a]] -> [a]
simpleFlatten []              = []
simpleFlatten ([]:xxs)        = simpleFlatten xxs
simpleFlatten ((y:ys):xxs)    = y : simpleFlatten (ys:xxs)


flatten :: NestedList a -> [a]
flatten (Elem x)        = [x]
flatten (List [])       = []
flatten (List (x:xs))   = flatten x ++ flatten (List xs)


-- 8

f :: (Eq a) => [a] -> [a]
f []        = []
f (x:xs)    = foldl (\(a:acc) z -> if z == a then acc else (z:a:acc)) [x] xs



compress :: (Eq a) => [a] -> [a]
compress [] = []
compress xs = myReverse $ loop xs []
    where
        loop []         acc     = acc
        loop (x:xs)     []      = loop xs [x]
        loop (x:xs)     (y:ys)  = loop xs (if x == y then (y:ys) else (x:y:ys))










{--


  case head of
    Elem x        -> x : (flatten $ List xs)
    List xxs      -> (flatten $ List xxs) : (flatten (List xs))

flatten xxs = simpleFlatten $ flatten' xxs
    where
      flatten' (List []) = []
      flatten' (Elem x) = [x]
      flatten' (List xs) = (flatten' x) : (fmap flatten' xs)
--}

{--
testFlatten :: IO ()
testFlatten = do
--  let nn = flatten $ List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]
  let ff = flatten $ List [ Elem 1, List [Elem 2, Elem 3], Elem 4 ]
  putStrLn $ "flatten " ++ (show nn) ++ " ~> \n\t" ++ (show ff) ++ "." 
--}



{--
flatten xxs = loop xxs [] []
  where
    loop [] innerAcc outerAcc = innerAcc : outerAcc
    loop (head : xxs) innerAcc outerAcc =
      case head of
        Elem x    -> loop xss (x : innerAcc) outerAcc
        List nnx  -> loop xss [] (
          --}
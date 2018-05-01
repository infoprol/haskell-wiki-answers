


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
data NestedList a = Elem a | List [NestedList a]

simpleFlatten :: [[a]] -> [a]
simpleFlatten []              = []
simpleFlatten ([]:xxs)        = simpleFlatten xxs
simpleFlatten ((y:ys):xxs)    = y : simpleFlatten (ys:xxs)




{--
    https://wiki.haskell.org/99_questions/1_to_10

    some functions given incomplete definitions...
--}


-- 1
myLast :: [a] -> a
myLast (x:[])       = x
myLast (x:xs)       = myLast xs


-- 2
myButLast :: [a] -> a
myButLast (x:y:[])      = x
myButLast (x:y:xs)      = myButLast (y:xs)


-- 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 0  = x
elementAt (x:xs) k  = elementAt xs (k - 1)


-- 4
myLength :: [a] -> Int
myLength = foldl (\acc x -> acc + 1) 0

-- 5
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []


-- 6
isPalindrome :: [a] -> Bool
isPalindrome xs = elemwiseEq (myReverse xs) xs
    where
        elemwiseEq [] [] = True
        elemwiseEq [] xs = False
        elemwiseEq xs [] = False
        elemwiseEq (x:xs) (y:ys) = x /= y && (elemwiseEq xs ys)





{--
doTest :: IO ()
doTest = do
    let ll = [42,420,666, 69, 666, 420, 42]
    putLine $ "myLast = " ++ (show . myLast $ ll) ++ "." 
    putLine $ "myLength = " ++ (myLength . show $ ll) ++ "."
--} 
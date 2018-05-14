module NNQs where

import System.IO

import System.Random
import Control.Monad (when)



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

-- 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = myReverse $ loop xs [] []
    where
        loop []         innAcc      outAcc  = innAcc : outAcc
        loop (x:xs)     []          outAcc  = loop xs [x] outAcc

        loop (x:xs) (y:ys) outAcc   | x == y    = loop  xs  (x:y:ys)    outAcc
                                    | otherwise = loop  xs  [x]         ((y:ys) : outAcc)

-- 10
encode :: Eq a => [a] -> [ (Int, a) ]
encode = g . pack
    where
        g = fmap $ \(x:xs) -> ((myLength (x:xs)), x)


{--
    https://wiki.haskell.org/99_questions/11_to_20
--}

len :: [a] -> Int
len = myLength


-- 11

data SymbolRun a = Single a | Multiple Int a deriving Show


encodeModified :: Eq a => [a] -> [SymbolRun a]
encodeModified = (fmap h) . pack
    where
        h (x:[]) = Single x
        h (x:xs) = Multiple (len (x:xs)) x


-- 12
decodeModified :: Eq a => [SymbolRun a] -> [a]
decodeModified [] = []
decodeModified (x:xs) =
  case x of
    (Single c)      ->  c : decodeModified xs
    (Multiple n c)  ->  fmap (const c) [ 1 .. n ] ++ decodeModified xs


-- 13
encodeDirect :: Eq a => [a] -> [SymbolRun a]
encodeDirect [] = []
encodeDirect zs = recurse zs []
  where
    recurse [] acc = myReverse acc
    recurse (x:xs) [] = recurse xs [Single x]
    recurse (x:xs)  (y:ys)  =
      case y of
        (Single c)      -> recurse xs (if c == x then Multiple 2 c :ys else (Single x):y:ys)
        (Multiple n c)  -> recurse xs (if c == x then Multiple (n+1) c :ys else (Single x):y:ys)

-- 14
xdupli :: [a] -> [a]
xdupli [] = []
xdupli xs = recurse xs []
  where
    recurse []      acc = foldl (\xs x -> x:xs) [] acc
    recurse (x:xs)  acc = recurse xs (x:x:acc)

xxdupli :: [a] -> [a]
xxdupli = dupl . rev
  where
    rev = foldl (flip (:)) []
    dupl = foldl (\xs x -> x:x:xs) []

-- oh, yeah - right...
dupli :: [a] -> [a]
dupli = foldr (\x xs -> x:x:xs) []




-- 15
rev :: [a] -> [a]
rev = foldl (flip (:)) []

repli :: [a] -> Int -> [a]
repli xxs n = foldr (++) [] zzs
  where
    zzs     = fmap repl xxs
    repl x  = take n (repeat x)

--repli xxs n = foldr (\x xs -> (take n $ repeat x) ++ xs) xxs []
--repli xs n = repli (dupli xs) (n - 1)
--repli xxs n = foldr (\x xs -> [ x | x <- 1 ... n ])


-- 16
{-- --}
-- deviating from prob signature of `:: [a] -> Int -> [a]`
--dropEvery :: Int -> [a] -> [a]

dropEvery :: [a] -> Int -> [a]
dropEvery xxs n = rev (recurse xxs [] [])
  where
    recurse []      innAcc  outAcc  = foldr (:) innAcc outAcc
    recurse (x:xs)  innAcc  outAcc
      | len innAcc < n - 1      = recurse xs (x:innAcc) outAcc
      | otherwise               = recurse xs [] (foldr (:) innAcc outAcc)




-- 17
{--
*Main> split "abcdefghik" 3
("abc", "defghik")
--}

split :: [a] -> Int -> [[a]]
split xs n = recurse xs n []
  where
    recurse []      _   acc = [rev acc, []]
    recurse xs      0   acc = [rev acc, xs]
    recurse (x:xs)  n   acc = recurse xs (n - 1) (x:acc)




-- 18
{--
slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-> "cdefg"
--}

slice :: [a] -> Int -> Int -> [a]
slice xs a b = rev $ loop xs a (b - a + 1) []
  where
    loop [] _ _ acc = acc
    loop (x:xs) a n acc
      | n < 1             = acc
      | a == 1            = loop xs   1         (n - 1)   (x:acc)
      | otherwise         = loop xs   (a - 1)   n         acc




-- 19
  {--
    *Main> rotate ['a','b','c','d','e','f','g','h'] 3
    "defghabc"
 
    *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
    "ghabcdef"
  --}

rotate :: [a] -> Int -> [a]
rotate []     _ = []
rotate (x:xs) n
  | n == 0    = x:xs
  | n >  0    = rotate (xs ++ [x]) (n - 1)
  | n <  0    = rev $ rotate (rev $ x:xs) (-1 * n)




-- 20
  {--
    *Main> removeAt 2 "abcd"
    
      ~> ('b',"acd")
  --}

--data Maybe a = Just a | Nothing

removeAt :: Int -> [a] -> ( Maybe a, [a] )
removeAt _ [] = (Nothing, [])
removeAt n xs = loop n xs []
  where
    loop _  []      acc = ( Nothing, rev acc )
    loop 1  (x:xs)  acc = ( Just x, rev acc ++ xs )
    loop n  (x:xs)  acc = loop (n - 1) xs (x:acc)






{-- https://wiki.haskell.org/99_questions/21_to_28 --}


-- 21
  {--
    P21> insertAt 'X' "abcd" 2
    "aXbcd"
  --}

insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt y xs n = loop y xs n []
  where
    loop y xs     1 acc = rev acc ++ (y:xs)
    loop y []     _ acc = loop y [] 1 acc
    loop y (x:xs) n acc = loop y xs (n - 1) (x:acc)





-- 22
  {--
    Prelude> range 4 9
      ~> [4,5,6,7,8,9]
  --}

range :: Int -> Int -> [Int]
range a b
  | b < a       = []
  | otherwise   = a : range (a + 1) b





-- 23
  {--
    Extract a given number of randomly selected elements from a list.
      Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
        ~> eda
  --}


{-- drops the nth element and returns the resulting list.  nth ZERO-BASED index--}
all_but :: [a] -> Int -> [a]
--all_but [] n = []
all_but xs n = loop xs n []
  where
    loop []     _   acc = rev acc
    loop (x:xs) 0   acc = rev acc ++ xs
    loop (x:xs) n   acc = loop xs (n - 1) (x:acc)




rnd_select :: [a] -> Int -> IO [a]
--rnd_select []   n = return [] :: IO [a]
--rnd_select xs   0 = return [] :: IO [a]
rnd_select xs n = do
  ans <- loop xs n []
  return ans
 
  where
    loop xs n acc = do
      let len = length xs
      if len < 1 || n < 1
        then return acc
        else do
          _ <- newStdGen
          gen <- getStdGen
          let (indx, _) = randomR (0, len - 1) gen :: (Int, StdGen)
          let x = xs !! indx
          loop (all_but xs indx) (n - 1) (x:acc)
          
           
        
      
      
  
--    loop []       n gen acc = return acc
--    loop (x:xs)   0 gen acc = return acc
    
  
  


{--
rnd_select xs n | length xs < 1 || n < 1 = pure []
                | otherwise = loop
  where loop = do
    r <- getStdGen
    let (indx, nextR) = randomR (0, (length xs - 1)) r
    setStdGen nextR
    return ( xs !! indx ) : rnd_select $ (removeAt indx xs) (n - 1) }
        --}
    

  

  
  
  
  
-- 24
  {--
    Lotto: Draw N different random numbers from the set 1..M.
      Prelude System.Random>diff_select 6 49
        ~> Prelude System.Random>[23,1,17,33,21,37]
  --}
  
  
-- 25
  {--
    Generate a random permutation of the elements of a list.
      Prelude System.Random>rnd_permu "abcdef"
      ~> Prelude System.Random>"badcef"
  --}

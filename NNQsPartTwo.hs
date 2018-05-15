module NNQsPartTwo where

import System.IO

import System.Random
import Control.Monad (when)

import NNQs

  
  
  
-- 24
  {--
    Lotto: Draw N different random numbers from the set 1..M.
      Prelude System.Random>diff_select 6 49
        ~> Prelude System.Random>[23,1,17,33,21,37]
  --}
  
diff_select :: Int -> Int -> IO [Int]
diff_select n m = rnd_select (range 1 m) n

  
-- 25
  {--
    Generate a random permutation of the elements of a list.
      Prelude System.Random>rnd_permu "abcdef"
      ~> Prelude System.Random>"badcef"
  --}
rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)



-- 26
    {--
        > combinations 3 "abcdef"
        ["abc","abd","abe",...]
    --}


-- 0-based, a <= i < b (python-style) indexing
slice' :: [a] -> Int -> Int -> [a]
slice' xs a b = slice xs (a + 1) b


powerset :: [a] -> [[a]]
powerset []         = [[]]
powerset (x:[])     = [ [], [x] ]
powerset (x:xs)     = [ x:ys | ys <- yys ] ++ yys where yys = powerset xs


-- CLAIM: this is not the most efficient way to do this.
combinations :: [a] -> Int -> [[a]]
combinations xs n = [ ys | ys <- powerset xs, length ys == n ]
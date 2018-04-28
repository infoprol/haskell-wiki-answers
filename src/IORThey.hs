module IORThey
    ( doPoetry
    ) where




import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (chr)

import Control.Monad
import Control.Applicative






doPoetry = do
    h <- openFile "hyperion.1.txt" ReadMode
    hGetLine h >>= putStrLn . show . words
    hClose h


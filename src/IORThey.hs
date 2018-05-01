{--
    taken from chapter 2 of <i>Haskell Design Patterns</i>
    by Ryan Lemmer (PACKT).

--}


module IORThey
    ( doPoetry
    ) where




import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (chr)

import Control.Monad
import Control.Applicative





data Chunk  = Chunk     { chunk :: String }
            | LineEnd   { chunk :: String, remainder :: String }
    deriving (Show)            





parseChunk chunk =
    let
        (leftS, rightS) = B8.break (== '\n') chunk
        toS = map (chr . fromEnum) . B.unpack
    in
    if rightS == B8.pack ""
        then Chunk      (toS leftS)
        else LineEnd    (toS leftS) ((toS . B8.tail) rightS)











doPoetry = do
    h <- openFile "hyperion.1.txt" ReadMode

    loop h
    hClose h
    where
        loop h' = do
            isEof <- hIsEOF h'
            if isEof
                then putStrLn "doneyet!"
            else
                hGetLine h' >>= putStrLn . show . words >> loop h'

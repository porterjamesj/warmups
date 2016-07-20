import Data.List
import Data.Char
import Control.Monad


-- run length encode

rle :: String -> String
rle = join . (map pairToString) . reverse . toRuns
  where
    pairToString (c, n) = [c, intToDigit n]
    toRuns = foldl build []
    build ((c, n):rest) next
      | c == next = (c, n+1):rest
      | otherwise = (next, 1):(c,n):rest
    build [] next = [(next, 1)]

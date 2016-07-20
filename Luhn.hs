import Data.List
import Data.Char
import Data.Maybe
import Data.List.Split
import qualified Data.Map as M
import Control.Monad
import Debug.Trace


-- Luhn algorithm

digits :: Int -> [Int]
digits = map (read . return) . show

luhn :: [Int] -> Bool
luhn xs = 0 == total `mod` 10
  where total = sum processed
        processed = zipWith ($) (cycle [id, (sum . digits . (*2))]) (reverse xs)

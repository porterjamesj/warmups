import Data.List
import Data.Maybe
import Control.Monad
import Debug.Trace

-- change problem

changeOrdered :: [Int] -> Int -> Maybe [Int]
changeOrdered [] goal = Nothing
changeOrdered _ 0 = Just []
changeOrdered (coin:rest) goal = case changeOrdered (trace ("rest: " ++ show rest) rest) (goal-coin) of
  Nothing -> Nothing
  Just coins -> Just ([coin] ++ coins)


change :: [Int] -> Int -> Maybe [Int]
change coins goal = join $ find isJust attempts
  where attempts = map (uncurry changeOrdered) (zip (permutations coins) (repeat goal))

-- Luhn algorithm

digits :: Int -> [Int]
digits = map (read . return) . show

luhn :: [Int] -> Bool
luhn xs = 0 == total `mod` 10
  where total = sum processed
        processed = zipWith ($) (cycle [id, (sum . digits . (*2))]) (reverse xs)

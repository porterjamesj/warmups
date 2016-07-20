import Data.List
import Data.Char
import Data.Maybe
import Control.Monad


-- change problem

changeOrdered :: [Int] -> Int -> Maybe [Int]
changeOrdered [] goal = Nothing
changeOrdered _ 0 = Just []
changeOrdered (coin:rest) goal = case changeOrdered rest (goal-coin) of
  Nothing -> Nothing
  Just coins -> Just ([coin] ++ coins)


change :: [Int] -> Int -> Maybe [Int]
change coins goal = join $ find isJust attempts
  where attempts = map (uncurry changeOrdered) (zip (permutations coins) (repeat goal))

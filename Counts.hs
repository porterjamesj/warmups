import Data.Map (fromListWith, keys, filterWithKey)

counts :: [Int] -> IO ()
counts = (mapM_ putStrLn) .
  (map show) .
  keys .
  (filterWithKey inRange) .
  (fromListWith (+)) .
  (map (\x -> (x,1)))
  where inRange n c = n <= c && c <= 2*n

import Data.List
import Data.List.Split

-- astoclock

data TimeSystem = TimeSystem {
  secondsPerMinute :: Int,
  minutesPerHour :: Int,
  hoursPerDay :: Int
} deriving Show


astroclock :: TimeSystem -> String -> String -> Int
astroclock ts t1 t2 = abs $ toSecs t1 - toSecs t2
  where
    spm = secondsPerMinute ts
    sph = (minutesPerHour ts) * spm
    conversions = [(*sph),(*spm),id]
    timeNums = (map read) . splitOn ":"
    toSecs = sum . (zipWith ($) conversions) . timeNums

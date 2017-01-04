module HackerrankxcomZchallengesZminizmaxzsum where
import Control.Monad
nums = [1,2,3,4,5]
out  = [10,14]

ignore :: Integer -> [Integer] -> [Integer]
ignore x q = do 
  y <- q 
  guard (x /= y)
  return y

input = foldr (\int out -> ignore int nums : out ) [] nums

data MaxMin  = MaxMin {
  mm_max :: Integer,
  mm_min :: Integer
} deriving Show

mm_empty init = MaxMin init init

answer = foldl go zed input
  where
    go (MaxMin max' min') set = let sum' = sum set in MaxMin (max max' sum') (min min' sum')
    zed  = mm_empty (sum $ head input)

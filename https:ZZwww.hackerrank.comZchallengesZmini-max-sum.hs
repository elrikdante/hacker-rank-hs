--module HackerrankxcomZchallengesZminizmaxzsum where
module Main where
import Control.Monad
import System.IO

ignore :: Integer -> [Integer] -> [Integer]
ignore = go False
  where
    go False _ []     = []
    go False x (y:ys) = if x == y then go True x ys else y : go False x ys
    go True  _ ys     = ys

inputs raw = foldr (\int out -> ignore int raw : out ) [] raw

data MaxMin  = MaxMin {
  mm_max :: Integer,
  mm_min :: Integer
}
instance Show MaxMin where show (MaxMin max min) = show min ++ " " ++ show max

mm_empty init = MaxMin init init

answer raw = foldl go zed raw' 
  where
    go (MaxMin max' min') set = 
      let sum' = sum set 
      in MaxMin (max max' sum') (min min' sum')
    zed  = mm_empty (sum $ head raw')
    raw' = (inputs raw)

main :: IO ()
main = (fmap (fmap read . words) getLine :: IO [Integer]) >>= putStrLn . show . answer 

module Main where -- httpsZZZwwwZhackerrankZcomZchallengesZsockZmerchant.hs
import qualified Data.Map.Strict as Data.Map.Strict
import qualified Data.List as Data.List
import Data.Either
readMany :: Read a => IO [a]
readMany = fmap (fmap read . words) getLine

countPairs :: [Integer] -> [Either Integer Int]
countPairs [] = []
countPairs (x:y:zs)
  | x == y    = Right 1 : countPairs zs
  | otherwise = Left x  : countPairs (y:zs)
countPairs (y:[])  = [Left y]
main = do
  numSocks <- fmap read getLine                              :: IO Int
  socks    <- fmap (Data.List.sort . take numSocks) readMany :: IO [Integer]
  print . sum . rights . countPairs $ socks

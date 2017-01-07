module Main where --httpsZZZwwwZhackerrankZcomZchallengesZbetweenZtwoZsetsZ
import Control.Monad

isFactor       :: Int -> Int -> Bool
x `isFactor` w = (w `mod` x) == 0

between children parents = do
  i <- [lowerB .. upperB]
  guard $ all (`isFactor` i) children
  guard $ all (i `isFactor`) parents
  return i
  where
    lowerB = minimum children
    upperB = maximum parents

readInts = fmap (fmap read . words) getLine :: IO [Int]

main = do
  (n:m:_)      <-    readInts
  ns           <-    readInts
  ms           <-    readInts
  putStrLn $ show $ length (between ns ms)

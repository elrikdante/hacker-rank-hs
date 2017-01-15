module Main where -- httpsZZZwwwZhackerrankZcomZchallengesZangryZprofessor.hs
import Control.Monad
import Data.Either
readMany :: Read a => IO [a]
readMany = fmap (fmap read . words) getLine

onTime :: [Int] -> [Int]
onTime = filter (<=0)

main = do
  testCases <- fmap read getLine :: IO Int
  forM_ [1..testCases] $ \_ -> do
    (count:cutoff:_)     <- readMany :: IO [Int]
    arrivals             <- readMany :: IO [Int]
    case (length (onTime arrivals) >= cutoff) of 
      False -> putStrLn "YES"
      True  -> putStrLn "NO"

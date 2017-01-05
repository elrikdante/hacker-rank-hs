module Main where -- module HttpsZZzwwwZhackerrankZcomZchallengesZappleZandZorange
import System.IO

inRange :: (Int, Int) -> Int -> Bool
inRange (lower,upper) x
  | x >= lower && x <= upper = True
  | otherwise                = False
readInts :: IO [Int]
readInts = fmap ((fmap read) . words) getLine :: IO [Int]

data Fruit = Apple Int Int | Orange Int Int deriving Show

main = do
  (lowerB:upperB:_)      <- readInts
  (appleP:orangeP:_)     <- readInts
  (appleLen:orangeLen:_) <- readInts
  appleOffsets           <- readInts
  orangeOffsets          <- readInts
  let 
    (applesOnHouse,orangesOnHouse)  = 
      foldl 
      (go (lowerB,upperB)) 
      (appleLen,orangeLen)  
      ((fmap (Apple appleP) appleOffsets) ++ (fmap (Orange orangeP) orangeOffsets) )

  putStrLn $ show applesOnHouse
  putStrLn $ show orangesOnHouse

  where
    go r (applesLeft,orangesLeft) (Apple t a)
      | inRange r (t + a) = (applesLeft,orangesLeft)
      | otherwise         = (applesLeft - 1, orangesLeft)
    go r (applesLeft,orangesLeft) (Orange t o)
      | inRange r (t + o) = (applesLeft,orangesLeft)
      | otherwise         = (applesLeft,orangesLeft - 1)
  

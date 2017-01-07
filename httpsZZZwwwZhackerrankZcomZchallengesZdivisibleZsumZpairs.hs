module Main where -- httpsZZZwwwZhackerrankZcomZchallengesZdivisibleZsumZpairs
import Control.Monad
import Data.Monoid
pairs xs = go 0 xs
  where
    go idx lst
      | idx == length xs = []
      | otherwise        = let x = lst !! idx 
                           in zipWith (,) (repeat x) lst : go (succ idx) lst

answers raw pivot = do
  let (min,max) = (0,length raw - 1)
      pointers  = mconcat $ pairs ([min..max])

  (i,j) <- pointers

  guard (i < j)

  let ai = raw !! i
      aj = raw !! j

  guard ((ai + aj) `mod` pivot == 0)

  return (i,j)

readInts = fmap (fmap read . words) getLine :: IO [Int]

main = do
  (_:pivot:_) <- readInts
  result      <- fmap (flip answers pivot) readInts
  print $ length result

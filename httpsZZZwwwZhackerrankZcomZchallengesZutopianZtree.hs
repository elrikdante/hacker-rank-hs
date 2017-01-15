module Main where -- httpsZZZwwwZhackerrankZcomZchallengesZsockZmerchant.hs
import Control.Monad
data Tree = Tree Int
grow x 0 = x * 2
grow x 1 = x + 1

height :: Tree -> Int -> Int -> Int
height (Tree h) g j 
  | g == j  = h
  |otherwise= let h'= grow h (g `mod` 2) 
              in height (Tree h') (succ g) j

readMany :: Read a => IO [a]
readMany = fmap (fmap read . words) getLine

main = do
  testCases <- fmap read getLine :: IO Int
  forM_ [0..testCases] $ \_ -> do
    cycles     <- fmap read getLine :: IO Int
    print $ height (Tree 1) 0 cycles

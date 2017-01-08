{-# LANGUAGE EmptyDataDecls,BangPatterns #-}
module Main where -- httpsZZZwwwZhackerrankZcomZchallengesZarrayZsplitting.hs
import Prelude hiding (partition)
import Data.Maybe
import Control.Monad
array        = [2, 2, 2, 2]
array2       = [1,0, 0, 1]
array3       = [4, 1, 0, 1, 1, 0, 1]
array4       = [3,3,3]
partition (x:y:[])
  | x == y    = Just ([x],[y])
  | otherwise = Nothing
partition [x]  = Nothing
partition (xs) = go (0, []) (length xs -1, [])
  where go (!i, !ys) (!j, !zs)
          | i > j                = 
            case rweight == lweight of
            True  -> Just (ys, zs)
            False -> Nothing
          | rweight' == lweight' = 
            case (compare y z) of 
              GT -> go (i , ys)        (j + joff, zs')
              EQ -> go (i, ys)         (j + joff, zs')
              LT -> go (i + ioff, ys') (j, zs)
          | lweight' > rweight'  = go (i, ys)         (j + joff, zs')
          | rweight' > lweight'  = go (i + ioff, ys') (j,zs)
          where
            y       = xs !! i
            z       = xs !! j
            ys'     = ys ++ [y]
            zs'     = z:zs
            ioff
              | lweight' > rweight' = 0
              | otherwise           = 1
            joff
              | rweight' > lweight' = 0
              | otherwise           = -1
            (lweight,lweight') = (sum ys, sum ys')
            (rweight,rweight') = (sum zs, sum zs')

score = go 0
  where 
    go score xs =
      case partition xs  of 
      Just (left, right) -> max (go (succ score) left) (go (succ score) right)
      Nothing            -> score

main = do
  cnt <- fmap (read :: String -> Int) getLine
  forM_ [1..cnt] $ \_ -> do
    len   <- fmap (read :: String -> Int) getLine
    ints  <- fmap (fmap read . words) getLine
    print $ score ints

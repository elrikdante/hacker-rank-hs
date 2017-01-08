{-# LANGUAGE EmptyDataDecls,BangPatterns #-}
module Main where -- httpsZZZwwwZhackerrankZcomZchallengesZarrayZsplitting.hs
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (partition)
import Data.Maybe
array        = [2, 2, 2, 2]
array2       = [1,0, 0, 1]
array3       = [4, 1, 0, 1, 1, 0, 1]
partition [x]  = Nothing
partition (xs) = go (0, []) (length xs -1, [])
  where go !(!i, !ys) !(!j, !zs)
          | i > j                = 
            case rweight == lweight of
            True  -> Just (ys, zs)
            False -> Nothing
          | rweight' == lweight' = go (i + ioff, ys') (j + joff, zs')
          | lweight' > rweight'  = go (i, ys)         (j + joff, zs')
          | rweight' > lweight'  = go (i + ioff, ys') (j,zs)
          where
            y       = xs !! i
            z       = xs !! j
            ys'     = y:ys
            zs'     = z:zs
            ioff
              | lweight' > rweight' = 0
              | otherwise           = 1
            joff
              | rweight' > lweight' = 0
              | otherwise           = -1
            (lweight,lweight') = (sum ys, sum ys')
            (rweight,rweight') = (sum zs, sum zs')


            
          

  

  

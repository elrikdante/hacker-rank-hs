{-# LANGUAGE EmptyDataDecls,BangPatterns #-}
module Main where -- httpsZZZwwwZhackerrankZcomZchallengesZarrayZsplitting.hs
import Prelude hiding (partition, (!))
import Data.Maybe
import Control.Monad
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as Data.Vector.Unboxed

array,array2,array3,array4 :: Data.Vector.Unboxed.Vector Int
array        = Data.Vector.Unboxed.fromList [2, 2, 2, 2]
array2       = Data.Vector.Unboxed.fromList [1,0, 0, 1]
array3       = Data.Vector.Unboxed.fromList [4, 1, 0, 1, 1, 0, 1]
array4       = Data.Vector.Unboxed.fromList [3,3,3]

partition xs
  | Data.Vector.Unboxed.length xs == 1 = Nothing
  | Data.Vector.Unboxed.length xs == 2 = 
    let 
      x = xs ! 0
      y = xs ! 1
    in case x == y of
      True  ->  Just (Data.Vector.Unboxed.singleton x, Data.Vector.Unboxed.singleton y)
      False ->  Nothing
  | otherwise                          = go 
                                         (0                               , Data.Vector.Unboxed.empty) 
                                         (Data.Vector.Unboxed.length xs -1, Data.Vector.Unboxed.empty)
  where go (!i, !ys) (!j, !zs)
          | i > j                = 
            case rweight == lweight of
            True  -> Just (ys, zs)
            False -> Nothing
          | rweight' == lweight' = 
            case (compare y z) of 
              GT ->                go (i       , ys)  (j + joff, zs')
              EQ ->                go (i       , ys)  (j + joff, zs')
              LT ->                go (i + ioff, ys') (j       , zs)
          | lweight' > rweight'  = go (i       , ys)  (j + joff, zs')
          | rweight' > lweight'  = go (i + ioff, ys') (j       , zs)
          where
            y       = xs ! i
            z       = xs ! j
            ys'     = Data.Vector.Unboxed.snoc ys y
            zs'     = Data.Vector.Unboxed.cons z zs
            ioff
              | lweight' > rweight' = 0
              | otherwise           = 1
            joff
              | rweight' > lweight' = 0
              | otherwise           = -1
            (lweight,lweight') = (Data.Vector.Unboxed.sum ys, Data.Vector.Unboxed.sum ys')
            (rweight,rweight') = (Data.Vector.Unboxed.sum zs, Data.Vector.Unboxed.sum zs')

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
    ints  <- fmap (fmap read . words) getLine :: IO [Int]
    print $ score (Data.Vector.Unboxed.fromList ints)

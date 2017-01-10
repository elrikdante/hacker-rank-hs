{-# LANGUAGE BangPatterns #-}
module Main where -- httpsZZZwwwZhackerrankZcomZchallengesZarrayZsplitting.hs
import Prelude hiding (partition, (!))
import Data.Maybe
import Control.Monad
import Control.Applicative ((<$>))
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as Data.Vector.Unboxed

data Search a = Search {
  s_idx :: !Int ,
  s_sum :: !Int 
  } deriving Show

search i j = (Search i 0,
              Search j 0)

array,array2,array3,array4 :: Data.Vector.Unboxed.Vector Int
array        = Data.Vector.Unboxed.fromList [2, 2, 2, 2]
array2       = Data.Vector.Unboxed.fromList [1,0, 0, 1]
array3       = Data.Vector.Unboxed.fromList [4, 1, 0, 1, 1, 0, 1]
array4       = Data.Vector.Unboxed.fromList [3,3,3]
array5       = Data.Vector.Unboxed.fromList (take 55 $ repeat 0 :: [Int])
test         = score <$> [array, array2, array3, array4,array5]
partition xs
  | Data.Vector.Unboxed.length xs <  2 = Nothing
  | Data.Vector.Unboxed.length xs == 2 = 
    let 
      x = xs ! 0
      y = xs ! 1
    in case x == y of
      True  ->  Just (Data.Vector.Unboxed.singleton x, 
                      Data.Vector.Unboxed.singleton y)
      False ->  Nothing
  | Data.Vector.Unboxed.sum xs == 0    = Just (Data.Vector.Unboxed.splitAt 1 xs)
  | otherwise                          = let (left, right) = search 0 (Data.Vector.Unboxed.length xs -1)
                                         in go left right
  where go left@(Search i lweight) right@(Search j rweight)
          | i > j                = 
            case rweight == lweight of
            True  -> Just (Data.Vector.Unboxed.splitAt i xs)
            False -> Nothing
          | rweight' == lweight' = 
            case (compare y z) of 
              GT ->                go left  right'
              EQ ->                go left' right
              LT ->                go left' right
          | lweight' > rweight'  = go left  right'
          | rweight' > lweight'  = go left' right
          where
            right'  = right { s_idx = j + joff, s_sum = rweight'}
            left'   = left  { s_idx = i + ioff, s_sum = lweight'}
            y       = xs ! i
            z       = xs ! j
            ioff
              | lweight' > rweight' = 0
              | otherwise           = 1
            joff
              | rweight' > lweight' = 0
              | otherwise           = -1
            !lweight' = lweight + y 
            !rweight' = rweight + z

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

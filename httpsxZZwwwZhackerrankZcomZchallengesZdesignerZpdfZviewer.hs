module Main where -- module httpsxZZwwwZhackerrankZcomZchallengesZdesignerZpdfZviewer where
--https://www.hackerrank.com/challenges/designer-pdf-viewer?h_r=next-challenge
import Control.Applicative
import Control.Monad
import System.IO
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Data.Map.Strict

main :: IO ()
main = do
    heights <- fmap (fmap read . words) getLine :: IO [Int]
    words   <- fmap words getLine
    pairs   <- return $ zipWith (,) ['a'..'z'] heights
    forM_ words (print . mm_area pairs)

mm_area :: [(Char, Int)] -> String -> Int
mm_area dict str = 
  let 
    tbl    = Data.Map.Strict.fromAscList dict
    width  = length str
    heights= foldr (go tbl) [] str
  in maximum heights * width
  where
    go tbl char lst = 
      (fromJust $ Data.Map.Strict.lookup char tbl):lst

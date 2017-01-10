module Main where -- httpsZZZwwwZhackerrankZcomZcontestsZwZZZchallengesZtheZgreatZxor
import qualified Data.Bits as Data.Bits
import Control.Monad

set1 = (2,2,10)

answer n = do
  x <- [0 .. (n-1)]
  guard ((x `Data.Bits.xor` n) > n)
  return x


main = do
  queries <- fmap read getLine :: IO Int
  forM_ [1..queries] $ \_ -> do
      getLine >>= print . length . answer . (read :: String -> Int)
    
                                               

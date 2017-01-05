--module HackerrankxcomZchallengesZminizmaxzsum where
module Main where
import Control.Monad
import System.IO
import Prelude hiding (subtract,div)

add,subtract,mult,div :: Integer -> Integer -> Integer
add      = (+)
subtract = (-)

mult _ 0 = 0
mult a b = add a (mult a (b - 1))

div  _ 0 = error "Division 0 times"
div  a b 
  | a >= b     =  1 + (div (a-b) b)
  | otherwise = 0

main :: IO ()
main = putStrLn $ show $ add 1 2

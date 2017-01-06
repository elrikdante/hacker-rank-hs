module Main where -- httpsZZZwwwZhackerrankZcomZchallengesZkangaroo.hs
--https://www.hackerrank.com/challenges/kangaroo
data Answer  = YES | NO deriving Show
data Exp     = E (Int, Int) (Int, Int)
testInputY   = E (0,3) (4,2)
testInputN   = E (0,2) (5,3)
equation x v = x : [v*i+x | i <- [1..10001]]
answer (E (x,v) (y,w))
 | any (==True) $ zipWith (==) (equation x v) (equation y w)  = YES
 | otherwise                                                  = NO

main = do
  (x:v:y:w:_) <- fmap ( fmap read . words ) getLine :: IO ([Int])
  putStrLn . show $ answer (E (x,v) (y,w))

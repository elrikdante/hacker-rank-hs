module Main where -- httpsZZZwwwZhackerrankZcomZchallengesZbonZappetit.hs
data Balance = Even | Over Integer
instance Show Balance where
  show Even     = "Bon Appetit"
  show (Over v) = show v

items  = [3, 10, 2, 9] :: [Integer]

sharedItems k i lst@(x:xs)
  | i == k    = sharedItems k (succ i) xs
  | otherwise = x : sharedItems k (succ i) xs
sharedItems _ _ [] = []                

balance :: Integer -> Int -> [Integer] -> Balance
balance charged pivot cs
  | charged - expected == 0   = Even
  | otherwise                 = Over overage
  where
    overage  = charged - expected
    expected = sum (sharedItems pivot 0 cs) `div` 2

readMany :: Read a => IO [a]
readMany = fmap (fmap read . words) getLine

main = do
  (n:k:_) <- readMany :: IO [Integer]
  prices  <- (fmap $ take (fromIntegral n)) readMany :: IO [Integer]
  charged <- fmap read getLine :: IO Integer
  print $ balance charged (fromIntegral k) prices
  

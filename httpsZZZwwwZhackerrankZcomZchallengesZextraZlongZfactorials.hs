module Main where -- HttpsZZZwwwZhackerrankZcomZchallengesZextraZlongZfactorials
import Data.Monoid (Product(..))
import Data.Monoid
import qualified System.IO


factorialBits :: Integer -> Product Integer
factorialBits 0 = mempty
factorialBits n = Product n <> (factorialBits (pred n))

factorial = getProduct . factorialBits

main = System.IO.getLine >>= print . factorial . read

module Util.Bits where

import Control.Applicative
import Control.Category
import Data.Bits
import Data.Bool
import Data.Foldable (Foldable (..))
import Data.List ((++), concat, repeat, take, transpose)
import Data.Maybe (Maybe (..), fromMaybe)
import Prelude (($), (==), (+), (-), compare, fmap, fromIntegral)
import qualified Prelude
import Util

(.&¬) :: Bits a => a -> a -> a
a .&¬ b = a .&. complement b

setBits :: (Bits a, Prelude.Integral n, Alternative f) => a -> f n
setBits = altMap pure ∘ go 0
  where go n a | zeroBits == a `shiftR` fromIntegral n = []
               | True = bool id (n:) (testBit a (fromIntegral n)) (go (n+1) a)

interleaveBits :: (Bits a, Bits b) => [a] -> b
interleaveBits = fromListLE . concat . transpose . pad False . fmap toListLE
  where
    pad :: a -> [[a]] -> [[a]]
    pad a₀ ass = [take l $ as ++ repeat a₀ | as <- ass]
      where l = fromMaybe 0 $ maximumBy compare (length <$> ass)

fromListLE :: Bits a => [Bool] -> a
fromListLE = foldr (\ b a -> bool zeroBits (bit 0) b .|. shiftL a 1) zeroBits

toListLE :: Bits a => a -> [Bool]
toListLE a | Just l <- bitSizeMaybe a = testBit a <$> [0..l-1]
           | otherwise = go a
  where
    go a | zeroBits == a = []
         | otherwise = testBit a 0 : go (shiftR a 1)

fromListBE :: Bits a => [Bool] -> a
fromListBE = foldl' (\ a b -> shiftL a 1 .|. bool zeroBits (bit 0) b) zeroBits

toListBE :: FiniteBits a => a -> [Bool]
toListBE a = [testBit a (l - k) | let l = finiteBitSize a, k <- [1..l]]

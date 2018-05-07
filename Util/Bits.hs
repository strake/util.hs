module Util.Bits where

import Control.Applicative
import Data.Bits
import Data.Bool
import Prelude ((==), (+), fromIntegral, id)
import qualified Prelude
import Util

(.&¬) :: Bits a => a -> a -> a
a .&¬ b = a .&. complement b

setBits :: (Bits a, Prelude.Integral n, Alternative f) => a -> f n
setBits = altMap pure ∘ go 0
  where go n a | zeroBits == a `shiftR` fromIntegral n = []
               | True = bool id (n:) (testBit a (fromIntegral n)) (go (n+1) a)

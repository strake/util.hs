module Util.Bits where

import Data.Bits
import Data.Bool
import Prelude ((==), (+), fromIntegral, id)
import qualified Prelude

(.&¬) :: Bits a => a -> a -> a
a .&¬ b = a .&. complement b

setBits :: (Bits a, Prelude.Integral n) => a -> [n]
setBits = go 0 where go n a | zeroBits == a `shiftR` fromIntegral n = []
                            | True = bool id (n:) (testBit a (fromIntegral n)) (go (n+1) a)

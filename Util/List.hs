{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Safe #-}
module Util.List where

import Control.Applicative
import Data.Bool
import Data.List.NonEmpty (NonEmpty (..))
import Numeric.Natural
import Util

splitWhen :: (a -> Bool) -> [a] -> NonEmpty [a]
splitWhen p = go
  where
    go = \ case
        [] -> []:|[]
        a:(go -> as:|ass) | p a -> []:|as:ass | True -> (a:as):|ass

padLeft :: Natural -> a -> [a] -> [a]
padLeft n a as = dropLengthOf as (replicate n a) <|> as
  where dropLengthOf [] as = as
        dropLengthOf _  [] = []
        dropLengthOf (_:bs) (_:as) = dropLengthOf bs as

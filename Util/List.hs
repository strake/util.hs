{-# LANGUAGE ViewPatterns #-}

module Util.List where

import Data.Bool
import Data.List.NonEmpty (NonEmpty (..))

splitWhen :: (a -> Bool) -> [a] -> NonEmpty [a]
splitWhen p = go
  where
    go = \ case
        [] -> []:|[]
        a:(go -> as:|ass) | p a -> []:|as:ass | True -> (a:as):|ass

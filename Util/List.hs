{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Safe #-}
module Util.List where

import Control.Category (Category (..))
import Control.Applicative
import Data.Bool
import Data.Either (Either (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (Maybe (..))
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

zipWithRemaining :: (a -> b -> c) -> [a] -> [b] -> ([c], Maybe (Either (NonEmpty a) (NonEmpty b)))
zipWithRemaining f = go id
  where
    go k [] [] = (k [], Nothing)
    go k [] (b:bs) = (k [], Just (Right (b:|bs)))
    go k (a:as) [] = (k [], Just (Left  (a:|as)))
    go k (a:as) (b:bs) = go (k . (f a b :)) as bs

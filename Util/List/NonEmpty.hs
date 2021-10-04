module Util.List.NonEmpty where

import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty (..), (<|))

infixl 5 |:
(|:) :: (Foldable f) => f a -> a -> NonEmpty a
as |: a = case toList as of
    [] -> a :| []
    a':as -> a' <| (as |: a)

infixl 5 ++|
(++|) :: (Foldable f) => NonEmpty a -> f a -> NonEmpty a
as ++| bs = case toList bs of
    [] -> as
    b:bs -> as |: b ++| bs

infixr 5 |++
(|++) :: (Foldable f) => f a -> NonEmpty a -> NonEmpty a
as |++ bs = case toList as of
    [] -> bs
    a:as -> a <| as |++ bs

module Util where

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Bool
import Data.Function (flip)
import Data.Maybe

infixr 3 &=&
(&=&) :: Applicative p => (a -> p b) -> (a -> p c) -> a -> p (b, c)
f &=& g = (liftA2 ∘ liftA2) (,) f g

infixr 3 *=*
(*=*) :: Applicative p => (a1 -> p b1) -> (a2 -> p b2) -> (a1, a2) -> p (b1, b2)
(f *=* g) (x, y) = liftA2 (,) (f x) (g y)

tripleK :: Applicative p => (a1 -> p b1) -> (a2 -> p b2) -> (a3 -> p b3) -> (a1, a2, a3) -> p (b1, b2, b3)
tripleK f g h (x, y, z) = liftA3 (,,) (f x) (g y) (h z)

infixr 2 <||>
(<||>) :: Applicative p => p Bool -> p Bool -> p Bool
(<||>) = liftA2 (||)

infixr 3 <&&>
(<&&>) :: Applicative p => p Bool -> p Bool -> p Bool
(<&&>) = liftA2 (&&)

liftA4 :: (Applicative p) => (a -> b -> c -> d -> e) -> p a -> p b -> p c -> p d -> p e
liftA4 f x y z = (<*>) (liftA3 f x y z)

apMA :: Monad m => m (a -> m b) -> a -> m b
apMA f = join ∘ ap f ∘ pure

whileJust :: (Alternative f, Monad m) => m (Maybe a) -> (a -> m b) -> m (f b)
whileJust mmx f = mmx >>= maybe (pure empty) (f >=> (<$> whileJust mmx f) ∘ (<|>) ∘ pure)

list :: b -> (a -> [a] -> b) -> [a] -> b
list y _ []     = y
list _ f (x:xs) = f x xs

infixr 9 &, ∘, ∘∘

(∘) :: (Category p) => p b c -> p a b -> p a c
(∘) = (.)

(&) :: (Category p) => p a b -> p b c -> p a c
(&) = flip (∘)

(∘∘) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f ∘∘ g) x y = f (g x y)

infixl 0 `onn`
onn :: (a -> a -> a -> b) -> (c -> a) -> c -> c -> c -> b
onn f g x y z = f (g x) (g y) (g z)

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

snd3 :: (a, b, c) -> b
snd3 (_,y,_) = y

þrd3 :: (a, b, c) -> c
þrd3 (_,_,z) = z

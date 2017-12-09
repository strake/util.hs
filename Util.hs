module Util where

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Bool
import Data.Function (flip)
import Data.Functor.Classes
import Data.Maybe
import Data.Semigroup
import Data.Monoid
import Numeric.Natural

import Prelude (Enum (..), Bounded, Eq, Ord, Read, Show, Foldable, Traversable (..))

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

untilJust :: Monad m => m (Maybe a) -> m a
untilJust mmx = mmx >>= maybe (untilJust mmx) pure

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

replicate :: Alternative f => Natural -> a -> f a
replicate 0 _ = empty
replicate n a = pure a <|> replicate (pred n) a

replicateA :: (Applicative p, Alternative f) => Natural -> p a -> p (f a)
replicateA 0 _ = pure empty
replicateA n a = (<|>) . pure <$> a <*> replicateA (pred n) a

mtimesA :: (Applicative p, Semigroup a, Monoid a) => Natural -> p a -> p a
mtimesA n = unAp . stimes n . Ap

newtype Ap p a = Ap { unAp :: p a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Foldable, Traversable,
            Eq1, Ord1, Read1, Show1, Eq, Ord, Read, Show, Bounded, Enum)
instance (Applicative p, Semigroup a) => Semigroup (Ap p a) where (<>) = liftA2 (<>)
instance (Applicative p, Semigroup a, Monoid a) => Monoid (Ap p a) where
    mempty = pure mempty
    mappend = (<>)

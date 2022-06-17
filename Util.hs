{-# LANGUAGE Safe #-}
module Util where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.State (StateT (..), state, evalState)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Bits
import Data.Bool
import Data.Foldable hiding (maximumBy, minimumBy)
import Data.Function (($), flip)
import Data.Functor.Classes
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup
import Data.Tuple (snd, swap)
import Data.Monoid (Monoid (..))
import Numeric.Natural

import Prelude (Enum (..), Bounded, Eq (..), Ord (..), Read, Show, Traversable (..), Ordering (..), Char, Int, Word, (+), (-), fromIntegral, uncurry, zipWith)

infixr 3 &=&
(&=&) :: Applicative p => (a -> p b) -> (a -> p c) -> a -> p (b, c)
(&=&) = (liftA2 ∘ liftA2) (,)

infixr 3 *=*
(*=*) :: (Bitraversable f, Applicative p) => (a1 -> p b1) -> (a2 -> p b2) -> f a1 a2 -> p (f b1 b2)
(*=*) = bitraverse

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
whileJust mmx f = mmx >>= maybe (pure empty) (\ x -> (<|) <$> f x <*> whileJust mmx f)

untilJust :: Monad m => m (Maybe a) -> m a
untilJust mmx = mmx >>= maybe (untilJust mmx) pure

whenM :: Monad m => m Bool -> m () -> m ()
whenM p x = p >>= flip when x

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p x = p >>= flip unless x

list :: b -> (a -> [a] -> b) -> [a] -> b
list y f = list' y (liftA2 f NE.head NE.tail)

list' :: b -> (NonEmpty a -> b) -> [a] -> b
list' y _ []     = y
list' _ f (x:xs) = f (x:|xs)

infixr 9 &, ∘, ∘∘

(∘) :: (Category p) => p b c -> p a b -> p a c
(∘) = (.)

(&) :: (Category p) => p a b -> p b c -> p a c
(&) = flip (∘)

(∘∘) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f ∘∘ g) x y = f (g x y)

compose2 :: (a' -> b' -> c) -> (a -> a') -> (b -> b') -> a -> b -> c
compose2 φ f g x y = φ (f x) (g y)

compose3 :: (a' -> b' -> c' -> d) -> (a -> a') -> (b -> b') -> (c -> c') -> a -> b -> c -> d
compose3 φ f g h x y z = φ (f x) (g y) (h z)

infixl 0 `onn`, `onnn`
onn :: (a -> a -> a -> b) -> (c -> a) -> c -> c -> c -> b
onn f g x y z = f (g x) (g y) (g z)

onnn :: (a -> a -> a -> a -> b) -> (c -> a) -> c -> c -> c -> c -> b
onnn f g w x y z = f (g w) (g x) (g y) (g z)

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

snd3 :: (a, b, c) -> b
snd3 (_,y,_) = y

þrd3 :: (a, b, c) -> c
þrd3 (_,_,z) = z

infixr 0 ₪
(₪) :: a -> (a -> b) -> b
(₪) = flip id

infixl 4 <₪>
(<₪>) :: Functor f => f a -> (a -> b) -> f b
(<₪>) = flip fmap

replicate :: Alternative f => Natural -> a -> f a
replicate 0 _ = empty
replicate n a = a <| replicate (pred n) a

replicateA :: (Applicative p, Alternative f) => Natural -> p a -> p (f a)
replicateA 0 _ = pure empty
replicateA n a = (<|) <$> a <*> replicateA (pred n) a

mtimesA :: (Applicative p, Semigroup a, Monoid a) => Natural -> p a -> p a
mtimesA n = unAp . stimes n . Ap

newtype Ap p a = Ap { unAp :: p a }
  deriving (Foldable, Functor, Traversable)
  deriving (Eq, Ord, Read, Show, Bounded, Enum) via p a
  deriving (Applicative, Monad, Alternative, MonadPlus, Eq1, Ord1, Read1, Show1) via p
instance (Applicative p, Semigroup a) => Semigroup (Ap p a) where (<>) = liftA2 (<>)
instance (Applicative p, Semigroup a, Monoid a) => Monoid (Ap p a) where
    mempty = pure mempty
    mappend = (<>)

(!!?) :: Foldable f => f a -> Natural -> Maybe a
(!!?) = go . toList where go [] _ = Nothing
                          go (x:_) 0 = Just x
                          go (_:xs) n = go xs (pred n)

intercalate :: Semigroup a => a -> NonEmpty a -> a
intercalate a = sconcat . NE.intersperse a

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f x y = liftA2 (,) x y >>= uncurry f

bind3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bind3 f x y z = liftA3 (,,) x y z >>= uncurry3 f

traverse2 :: (Traversable t, Applicative t, Applicative p)
          => (a -> b -> p c) -> t a -> t b -> p (t c)
traverse2 f xs ys = sequenceA (f <$> xs <*> ys)

traverse3 :: (Traversable t, Applicative t, Applicative p)
          => (a -> b -> c -> p d) -> t a -> t b -> t c -> p (t d)
traverse3 f xs ys zs = sequenceA (f <$> xs <*> ys <*> zs)

foldMap2 :: (Foldable t, Applicative t, Monoid z)
         => (a -> b -> z) -> t a -> t b -> z
foldMap2 f xs ys = fold (f <$> xs <*> ys)

foldMap3 :: (Foldable t, Applicative t, Monoid z)
         => (a -> b -> c -> z) -> t a -> t b -> t c -> z
foldMap3 f xs ys zs = fold (f <$> xs <*> ys <*> zs)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (w, x, y, z) = f w x y z

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f w x y z = f (w, x, y, z)

infix 4 ∈, ∉
(∈), (∉) :: (Eq a, Foldable f) => a -> f a -> Bool
(∈) = elem
(∉) = not ∘∘ elem

maximumBy, minimumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
maximumBy f = foldr (\ a -> Just . fromMaybe a & \ b -> case f a b of GT -> a; _ -> b) Nothing
minimumBy f = foldr (\ a -> Just . fromMaybe a & \ b -> case f a b of LT -> a; _ -> b) Nothing

foldMapA :: (Applicative p, Monoid b, Foldable f) => (a -> p b) -> f a -> p b
foldMapA f = unAp . foldMap (Ap . f)

altMap :: (Alternative p, Foldable f) => (a -> p b) -> f a -> p b
altMap f = foldr ((<|>) . f) empty

altMapA :: (Applicative p, Alternative q, Foldable f) => (a -> p (q b)) -> f a -> p (q b)
altMapA f = fmap asum . traverse f . toList

asumA :: (Applicative p, Alternative q, Foldable f) => f (p (q a)) -> p (q a)
asumA = altMapA id

iterateM :: Monad m => Natural -> (a -> m a) -> a -> m (NonEmpty a)
iterateM 0 _ x = pure (x:|[])
iterateM k f x = (x NE.<|) <$> (f x >>= iterateM (pred k) f)

loopM :: MonadFix m => (a -> m (a, b)) -> m b
loopM f = fmap snd . mfix $ \ (a, _) -> f a

unlessMaybe, whenMaybe :: (Monad m, Alternative f) => Bool -> m a -> m (f a)
whenMaybe = \ case
    False -> \ _ -> pure empty
    True -> fmap pure
unlessMaybe = whenMaybe . not

infixl 3 <|, |>

(<|) :: Alternative f => a -> f a -> f a
x <| xs = pure x <|> xs

(|>) :: Alternative f => f a -> a -> f a
xs |> x = xs <|> pure x

count :: (Traversable f, Enum n) => f a -> f (n, a)
count = countFrom (toEnum 0)

countFrom :: (Traversable f, Enum n) => n -> f a -> f (n, a)
countFrom n = flip evalState n . traverse (\ a -> state $ \ k -> ((k, a), succ k))

foldMapWithIx :: (Foldable f, Enum k, Monoid b) => (k -> a -> b) -> f a -> b
foldMapWithIx f = fold . zipWith f [toEnum 0..] . toList

mapWithIx :: (Traversable f, Enum k) => (k -> a -> b) -> f a -> f b
mapWithIx f = runIdentity ∘ traverseWithIx (Identity ∘∘ f)

traverseWithIx :: (Applicative p, Traversable f, Enum k) => (k -> a -> p b) -> f a -> p (f b)
traverseWithIx f = traverse (uncurry f) ∘ count

some :: Alternative p => p a -> p (NonEmpty a)
some = liftA2 (:|) <*> many

unfoldrM :: (Monad m, Alternative f) => (a -> m (Maybe (b, a))) -> a -> m (f b)
unfoldrM f = go
  where
    go = f >=> \ case
        Nothing -> pure empty
        Just (x, z) -> (x <|) <$> go z

digit :: Char -> Maybe Word
digit = go & \ n -> n <$ guard (fromIntegral n >= (0 :: Int))
  where
    go x
      | dec < 10 = dec
      | abcl < 26 = abcl + 10
      | abcu < 26 = abcu + 10
      | otherwise = complement 0
      where
        dec = fromIntegral $ fromEnum x - fromEnum '0'
        abcl = fromIntegral $ fromEnum x - fromEnum 'a'
        abcu = fromIntegral $ fromEnum x - fromEnum 'A'
{-# INLINE digit #-}

slipr :: (a -> b -> c -> d) -> b -> c -> a -> d
slipr φ b c a = φ a b c

slipl :: (a -> b -> c -> d) -> c -> a -> b -> d
slipl φ c a b = φ a b c

slipr4 :: (a -> b -> c -> d -> e) -> b -> c -> d -> a -> e
slipr4 φ b c d a = φ a b c d

slipl4 :: (a -> b -> c -> d -> e) -> d -> a -> b -> c -> e
slipl4 φ d a b c = φ a b c d

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 φ c b a = φ a b c

flip4 :: (a -> b -> c -> d -> e) -> d -> b -> c -> a -> e
flip4 φ d b c a = φ a b c d

flip5 :: (a -> b -> c -> d -> e -> f) -> e -> b -> c -> d -> a -> f
flip5 φ e b c d a = φ a b c d e

flap :: Functor f => f (a -> b) -> a -> f b
flap fs a = ($ a) <$> fs

mwhen, munless :: Monoid a => Bool -> a -> a
mwhen False = mempty
mwhen True = id
munless = mwhen . not

applyWhen, applyUnless :: Bool -> (a -> a) -> a -> a
applyWhen = flip $ bool id
applyUnless = applyWhen . not

allM, anyM :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m Bool

allM f = go . toList
  where
    go []     = pure True
    go (a:as) = f a >>= bool (pure False) (go as)

anyM f = go . toList
  where
    go [] = pure False
    go (a:as) = f a >>= bool (go as) (pure True)

andM :: Monad m => m Bool -> m Bool -> m Bool
andM m1 m2 = m1 >>= bool (pure False) m2

orM :: Monad m => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= bool m2 (pure True)

mapAccumL :: Traversable t => (b -> a -> (b, c)) -> b -> t a -> (b, t c)
mapAccumL f = runIdentity ∘∘ mapAccumLM (Identity ∘∘ f)

mapAccumR :: Traversable t => (a -> b -> (b, c)) -> b -> t a -> (b, t c)
mapAccumR f = runIdentity ∘∘ mapAccumRM (Identity ∘∘ f)

mapAccumLM
 :: (Monad m, Traversable t)
 => (b -> a -> m (b, c))
 -> b -> t a -> m (b, t c)
mapAccumLM f s = fmap swap . flip runStateT s . traverse f'
  where
    f' = StateT ∘ fmap swap ∘∘ flip f

mapAccumRM
 :: (Monad m, Traversable t)
 => (a -> b -> m (b, c))
 -> b -> t a -> m (b, t c)
mapAccumRM f s = fmap swap . flip runStateT s . forwards . traverse f'
  where
    f' = Backwards ∘ StateT ∘ fmap swap ∘∘ f

whenNothing :: Applicative p => Maybe a -> p a -> p a
whenNothing = maybe id (pure . pure)

infixr `orElseM`
orElseM :: Monad m => m (Maybe b) -> m b -> m b
orElseM f g = f >>= maybe g pure

{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, TypeFamilies, UndecidableInstances, ScopedTypeVariables #-}
module Data.Vec
  ( Vec(..)
  , Fin(..)
  , head
  , tail
  , last
  , init
  , length
  , map
  , append
  , concat
  , interleave
  , interleave'
  , reverse
  , transpose
  , zipWith
  , zip
  , (!)
  , (!?)
  , take
  ) where

import Control.Applicative
import Data.Foldable hiding (concat)
import Data.Traversable
import Prelude hiding (foldr,head,tail,last,init,length,map,concat,
                       reverse,zip,zipWith,take)

-- Type-level natural numbers with addition and multiplication
data Z
data S n

type OneT = S Z
type TwoT = S OneT

type family a :+: b
type instance Z   :+: n = n
type instance S m :+: n = S (m :+: n)

type family x :*: y
type instance Z   :*: n = Z
type instance S m :*: n = n :+: (m :*: n)

class IsNat n where
  units :: Vec n ()

instance IsNat Z where
  units = Nil

instance IsNat n => IsNat (S n) where
  units = () :< units

infixr 5 :<

data Vec n a where
  Nil  :: Vec Z a
  (:<) :: a -> Vec n a -> Vec (S n) a

data Fin m n where
  FZero :: Fin Z (S n)
  FSucc :: Fin m n -> Fin (S m) (S n)

instance Eq a => Eq (Vec n a) where
  Nil       == Nil       = True
  (x :< xs) == (y :< ys) = x == y && xs == ys

instance Functor (Vec n) where
  fmap _ Nil       = Nil
  fmap f (x :< xs) = f x :< fmap f xs

instance Foldable (Vec n) where
  foldr _ y Nil       = y
  foldr f y (x :< xs) = x `f` foldr f y xs

instance IsNat n => Applicative (Vec n) where
  pure  = pureV
  (<*>) = applyV
  
instance IsNat n => Monad (Vec n) where
  return  = pureV
  x >>= f = joinV (f <$> x)

instance Traversable (Vec n) where
  traverse _ Nil       = pure Nil
  traverse f (x :< xs) = (:<) <$> f x <*> traverse f xs

instance Show a => Show (Vec n a) where
  showsPrec _ Nil       = showString "Nil"
  showsPrec p (x :< xs) = showParen (p > consPrecedence)   $
                          showsPrec (consPrecedence + 1) x .
                          showString " :< "                .
                          showsPrec consPrecedence xs
    where consPrecedence = 5

-- Used by the Foldable instance.
pureV :: IsNat n => a -> Vec n a
pureV x = const x <$> units

applyV :: Vec n (a -> b) -> Vec n a -> Vec n b
Nil       `applyV` Nil       = Nil
(f :< fs) `applyV` (x :< xs) = f x :< (fs `applyV` xs)

-- For the Monad instance.
joinV :: Vec n (Vec n a) -> Vec n a
joinV Nil       = Nil
joinV (x :< xs) = head x :< joinV (tail <$> xs)

natToInt :: forall n. IsNat n => n -> Int
natToInt = const $ length (units :: Vec n ())

-- Exported functions.

-- This is a total function :-)
head :: Vec (S n) a -> a
head (x :< _) = x

-- Same here.
tail :: Vec (S n) a -> Vec n a
tail (_ :< xs) = xs

last :: Vec (S n) a -> a
last (x :< Nil)         = x
last (_ :< xs@(_ :< _)) = last xs

init :: Vec (S n) a -> Vec n a
init (_ :< Nil)         = Nil
init (x :< xs@(_ :< _)) = x :< init xs

length :: Vec n a -> Int
length = foldl' (const . (+1)) 0

map :: (a -> b) -> Vec n a -> Vec n b
map = fmap

append :: Vec m a -> Vec n a -> Vec (m :+: n) a
append Nil       ys = ys
append (x :< xs) ys = x :< append xs ys

-- The :!: type operator is nearly the same as :+: except that the m and n
-- type parameters are flipped in the recursive case of the type family
-- instance, just like the parameters are flipped in the recursive case of
-- the interleave function.  I'm forced to define another type family matching
-- this recursion scheme rather than just using :+: because apparently, there
-- is no way to tell GHC that :+: is indeed commutative.
type family m :!: n
type instance Z   :!: n = n
type instance S m :!: n = S (n :!: m)	-- We flip m and n

interleave :: Vec m a -> Vec n a -> Vec (m :!: n) a
interleave Nil       ys = ys
interleave (x :< xs) ys = x :< interleave ys xs

-- A third way to define addition at the type-level, which matches the way
-- we recurse in a reverse function using an accumulating parameter.
type family m :^: n
type instance Z   :^: n = n
type instance S m :^: n = m :^: S n

reverse :: Vec n a -> Vec (n :^: Z) a
reverse as = go as Nil
  where go :: Vec m a -> Vec n a -> Vec (m :^: n) a
        go Nil       ys = ys
        go (x :< xs) ys = go xs (x :< ys)

-- Interleave two vectors of the same length.
interleave' :: Vec n a -> Vec n a -> Vec (n :*: TwoT) a
interleave' Nil       Nil       = Nil
interleave' (x :< xs) (y :< ys) = x :< y :< interleave' xs ys

transpose :: IsNat n => Vec m (Vec n a) -> Vec n (Vec m a)
transpose = sequenceA

concat :: Vec m (Vec n a) -> Vec (m :*: n) a
concat Nil         = Nil
concat (xs :< xss) = append xs (concat xss)

zipWith :: IsNat n => (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f xs ys = f <$> xs <*> ys

zip :: IsNat n => Vec n a -> Vec n b -> Vec n (a,b)
zip = zipWith (,)

(!) :: Vec n a -> Fin m n -> a
(x :< _)  ! FZero   = x
(_ :< xs) ! FSucc n = xs ! n

(!?) :: Vec n a -> Int -> Maybe a
Nil       !? _ = Nothing
(x :< _)  !? 0 = Just x
(_ :< xs) !? n = xs !? (n - 1)

take :: Fin m n -> Vec n a -> Vec m a
take FZero _             = Nil
take (FSucc n) (x :< xs) = x :< take n xs

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module CategoryTheoryInAction where

import Prelude hiding (Monoid, mappend, mempty, Functor, fmap, Applicative, return, Monad)

{- A category consists of objects and arrows `arr` between these objects. An arrow describes a morphism between to
   objects. The following laws have to be fulfilled:
      1. each object has an identity arrow `arr a a`
      2. arrows compose
      3. that composition is assocciative
   
   Here, our objects a all types and the arrows are described by a higher-kinded type `arr`
-}
class Category arr where
  identity :: arr a a

  compose :: arr a b -> arr b c -> arr a c

instance Category (->) where
  identity = \a -> a

  compose f g = g . f





{- Let's continue with operations on categories. 
   
   The first operation is a mapping of objects and arrows from one category into another. That
   transformation is known as Functor and follows the laws:
       1. associate to each object x in category C in object (m x) in category D
       2. associate to each arrow `arr a b` in C and arrow in D `arr' (m a) (m b)` and obey:
         a. m (arr a a) == arr (m a) (m a)
         b. m (g . f)   == (m g) `compose` (m f)
-}
class (Category arr, Category arr') => Functor arr arr' m | arr -> arr', m -> arr, m -> arr' where
  fmap :: arr a b -> arr' (m a) (m b)

class (Functor arr arr m) => EndoFunctor arr m

class (EndoFunctor (->) m) => HaskellFunctor m

-- Let's have an implementation example to see how it works:
instance Functor (->) (->) Maybe where
  fmap f = \fa -> case fa of
    Just a  -> Just . f $ a
    Nothing -> Nothing

instance EndoFunctor (->) Maybe

instance HaskellFunctor Maybe  





class (Category arr, Category arr', Category arr'') => BiFunctor arr arr' arr'' m where
  bimap :: arr a c -> arr' b d -> arr'' (m a b) (m c d)

class (BiFunctor (->) (->) (->) m) => HaskellBiFunctor m





{- A Functor is covariant - meaning you describe a mapping arr a b on m a. But what if we need a contravariant Functor where
   the mapping is inverted: arr b a on m a?
   
   To achieve that we need the opposite of a category which basically inverts the direction of all arrows.
-}
data Opposite arr a b where
  Opp :: (Category arr) => arr b a -> Opposite arr a b

instance (Category arr) => Category (Opposite arr) where
  identity = Opp $ identity

  compose (Opp f) (Opp g) = Opp $ compose g f

class (Functor (Opposite arr) arr m) => Contravariant arr m
-- fmap :: Opposite arr a b -> arr (m a) (m b)
--   '-> fmap :: arr b a -> arr (m a) (m b)

class (Contravariant (->) m) => HaskellContravariant m





-- Of course, now you can also have a construct like a ProFunctor which combines a contravariant and covariant Functor.
class (BiFunctor (Opposite arr) arr' (->) m) => ProFunctor arr arr' m

class (ProFunctor arr arr m) => HomFunctor arr m

class (HomFunctor (->) m) => HaskellProFunctor m





class (Functor arr arr' m) => ApplicativeFunctor arr arr' m where
  return :: arr a (m a)

  (<*>) :: m (arr a b) -> arr' (m a) (m b)

class (ApplicativeFunctor arr arr m) => ApplicativeEndoFunctor arr m

class (ApplicativeEndoFunctor (->) m) => HaskellApplicative m





class (ApplicativeEndoFunctor arr m) => Monad arr m where
  join :: m (m a) -> m a

class (Monad (->) m) => HaskellMonad m





newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance (Monad (->) m) => Category (Kleisli m) where
  identity = Kleisli $ return

  compose f g = Kleisli $ join . fmap (runKleisli g) . (runKleisli f)

class (Monad (->) m, EndoFunctor (Kleisli m) n) => Traverse m n
-- fmap :: Kleisli m a b -> Kleisli (n a) (n b)
--   '-> fmap :: (a -> m b) -> (n a -> m (n b))

class (Category arr) => Monoid arr a where
  mempty :: arr a a
  mempty = identity

  mappend :: arr a b -> arr b c -> arr a c
  mappend = compose

class (Monoid (->) a) => HaskellMonoid a

instance Monoid (->) Int

zero = mempty
one  = mappend (+ 1) zero :: Int -> Int
two  = mappend one one

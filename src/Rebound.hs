{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Rebound
  ( -- * Bind
    Bind,
    Bind1,
    pattern Bound,
    pattern Bound1,
    pattern Free,
    unbind,
    unbind1,

    -- * Optic combinators
    abstractOver,
    abstract1Over,
    instantiateOver,
    instantiate1Over,
    closedOver,
    maybeClosedOver,
    unsafeClosedOver,
    unusedOver,
    maybeUnusedOver,
    Traversal,
    ASetter,

    -- * Non-optic combinators
    abstract,
    abstract1,
    instantiate,
    instantiate1,
    closed,
    maybeClosed,
    unsafeClosed,
    unused,
    maybeUnused,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty)

-- | A binding parameterized by 'b'.
--
-- 'Bind' is just a synonym for 'Either'.
-- There are three corresponding pattern synonyms; 'Bound', 'Bound1', and 'Free'.
-- The goal of this library is to have as few dependencies as possible, and using synonyms instead of a new data type allows us to still get third-party instances for things like @Hashable@.
type Bind = Either

-- | A simple binding/binding over a single argument, like a lambda abstraction.
-- Most definitions in this library have special cases for single-argument abstractions.
type Bind1 = Bind ()

{-# COMPLETE Bound, Free #-}

{-# COMPLETE Bound1, Free #-}

-- | A bound variable parameterized by 'b'.
pattern Bound :: b -> Bind b a
pattern Bound b = Left b

-- | A simple/unparameterized binding.
pattern Bound1 :: Bind1 a
pattern Bound1 = Left ()

-- | A free variable
pattern Free :: a -> Bind b a
pattern Free a = Right a

{-# INLINE unbind #-}
unbind :: (b -> r) -> (a -> r) -> Bind b a -> r
unbind = either

{-# INLINE unbind1 #-}
unbind1 :: r -> (a -> r) -> Bind1 a -> r
unbind1 = unbind . const

-- Content

{-# INLINE abstractOver #-}
abstractOver :: ASetter s t a (Bind b a) -> (a -> Maybe b) -> s -> t
abstractOver t f = over t (\a -> maybe (Free a) Bound (f a))

{-# INLINE abstract1Over #-}
abstract1Over :: Eq a => ASetter s t a (Bind1 a) -> a -> s -> t
abstract1Over t a = over t (\s -> if a == s then Bound1 else Free s)

{-# INLINE instantiateOver #-}
instantiateOver :: ASetter s t (Bind b a) a -> (b -> a) -> s -> t
instantiateOver t f = over t (unbind f id)

{-# INLINE instantiate1Over #-}
instantiate1Over :: ASetter s t (Bind1 a) a -> a -> s -> t
instantiate1Over t sub = over t (unbind1 sub id)

{-# INLINE closedOver #-}
closedOver :: Traversal s t a b -> s -> Either (NonEmpty a) t
closedOver t = validationToEither . t (Failure . pure)

{-# INLINE maybeClosedOver #-}
maybeClosedOver :: Traversal s t a b -> s -> Maybe t
maybeClosedOver t = t (const Nothing)

{-# INLINE unsafeClosedOver #-}
unsafeClosedOver :: Show a => Traversal s t a b -> s -> t
unsafeClosedOver t = runIdentity . t (error . mappend "Unbound variable: " . show)

{-# INLINE unusedOver #-}
unusedOver :: Traversal s t (Bind b a) a -> s -> Either (NonEmpty b) t
unusedOver t = validationToEither . t (unbind (Failure . pure) pure)

{-# INLINE maybeUnusedOver #-}
maybeUnusedOver :: Traversal s t (Bind b a) a -> s -> Maybe t
maybeUnusedOver t = t (unbind (const Nothing) pure)

-- Non-optic

{-# INLINE abstract #-}
abstract :: Functor t => (a -> Maybe b) -> t a -> t (Bind b a)
abstract = abstractOver mapped

{-# INLINE abstract1 #-}
abstract1 :: (Eq a, Functor t) => a -> t a -> t (Bind1 a)
abstract1 = abstract1Over mapped

{-# INLINE instantiate #-}
instantiate :: Functor f => (b -> a) -> f (Bind b a) -> f a
instantiate = instantiateOver mapped

{-# INLINE instantiate1 #-}
instantiate1 :: Functor f => a -> f (Bind1 a) -> f a
instantiate1 = instantiate1Over mapped

{-# INLINE closed #-}
closed :: Traversable t => t a -> Either (NonEmpty a) (t b)
closed = closedOver traverse

{-# INLINE maybeClosed #-}
maybeClosed :: Traversable t => t a -> Maybe (t b)
maybeClosed = maybeClosedOver traverse

{-# INLINE unsafeClosed #-}
unsafeClosed :: (Show a, Traversable t) => t a -> t b
unsafeClosed = unsafeClosedOver traverse

{-# INLINE unused #-}
unused :: Traversable t => t (Bind b a) -> Either (NonEmpty b) (t a)
unused = unusedOver traverse

{-# INLINE maybeUnused #-}
maybeUnused :: Traversable t => t (Bind b a) -> Maybe (t a)
maybeUnused = maybeUnusedOver traverse

-- Lens stuff

-- | Traversal compatible most lens/optics libraries.
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

-- | Setter optic compatible most lens/optics libraries.
type ASetter s t a b = (a -> Identity b) -> (s -> Identity t)

{-# INLINE over #-}
over :: ASetter s t a b -> (a -> b) -> (s -> t)
over l f = runIdentity . l (Identity . f)

{-# INLINE mapped #-}
mapped :: Functor f => ASetter (f a) (f b) a b
mapped f = Identity . fmap (runIdentity . f)

-- Validation

data Validation e a = Failure (NonEmpty e) | Success a
  deriving (Functor)

instance Applicative (Validation e) where
  pure = Success
  Success f <*> Success a = Success (f a)
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e1 <*> _ = Failure e1
  _ <*> Failure e2 = Failure e2

{-# INLINE validationToEither #-}
validationToEither :: Validation e a -> Either (NonEmpty e) a
validationToEither (Failure e) = Left e
validationToEither (Success a) = Right a

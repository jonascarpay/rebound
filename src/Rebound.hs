{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Rebound
  ( -- * Bind
    Bind (..),
    Bind1,
    unbind,

    -- * Non-optic combinators
    abstract,
    abstract1,
    instantiate,
    instantiate1,
    closed,
    maybeClosed,
    unsafeClosed,

    -- * Optic combinators
    abstractOver,
    abstract1Over,
    instantiateOver,
    instantiate1Over,
    closedOver,
    maybeClosedOver,
    unsafeClosedOver,
  )
where

import Data.Functor.Identity
import Data.List.NonEmpty

data Bind b a = Bound b | Free a
  deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

type Bind1 = Bind ()

{-# INLINE unbind #-}
unbind :: (b -> r) -> (a -> r) -> Bind b a -> r
unbind f _ (Bound b) = f b
unbind _ f (Free a) = f a

-- Non-optic

{-# INLINE abstract #-}
abstract :: Functor t => (a -> Maybe b) -> t a -> t (Bind b a)
abstract = abstractOver mapped

{-# INLINE abstract1 #-}
abstract1 :: (Eq a, Functor t) => a -> t a -> t (Bind () a)
abstract1 = abstract1Over mapped

{-# INLINE instantiate #-}
instantiate :: Functor f => (b -> a) -> f (Bind b a) -> f a
instantiate = instantiateOver mapped

{-# INLINE instantiate1 #-}
instantiate1 :: Functor f => a -> f (Bind () a) -> f a
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

-- Over

{-# INLINE abstractOver #-}
abstractOver :: ASetter s t a (Bind b a) -> (a -> Maybe b) -> s -> t
abstractOver t f = over t (\a -> maybe (Free a) Bound (f a))

{-# INLINE abstract1Over #-}
abstract1Over :: Eq a => ASetter s t a (Bind () a) -> a -> s -> t
abstract1Over t a = over t (\s -> if a == s then Bound () else Free s)

{-# INLINE instantiateOver #-}
instantiateOver :: ASetter s t (Bind b a) a -> (b -> a) -> s -> t
instantiateOver t f = over t (unbind f id)

{-# INLINE instantiate1Over #-}
instantiate1Over :: ASetter s t (Bind () a) a -> a -> s -> t
instantiate1Over t sub = over t (unbind (const sub) id)

{-# INLINE closedOver #-}
closedOver :: Traversal s t a b -> s -> Either (NonEmpty a) t
closedOver t = validationToEither . t (Failure . pure)

{-# INLINE maybeClosedOver #-}
maybeClosedOver :: Traversal s t a b -> s -> Maybe t
maybeClosedOver t = t (const Nothing)

{-# INLINE unsafeClosedOver #-}
unsafeClosedOver :: Show a => Traversal s t a b -> s -> t
unsafeClosedOver t = runIdentity . t (error . mappend "Unbound variable: " . show)

-- Lens stuff

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

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

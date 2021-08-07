module Rebound where

type Traversal s t a b = forall f. (a -> f b) -> (s -> f t)



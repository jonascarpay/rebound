# rebound
[![rebound on hackage](https://img.shields.io/hackage/v/rebound)](http://hackage.haskell.org/package/rebound)
[![rebound on Stackage Nightly](https://stackage.org/package/rebound/badge/nightly)](https://stackage.org/nightly/package/rebound)

_Make `bound` Succ more_

Tools for working with de Bruijn-indices.
Essentially just `bounds`'s '`Bound.Scope.Simple` module, i.e. the non-monadic version, but with extra tools for working with traversals.

In particular, it adds 

```haskell
abstract1Over    :: Eq a => ASetter s t a (Bind () a) -> a -> s -> t
abstractOver     :: ASetter s t a (Bind b a) -> (a -> Maybe b) -> s -> t
instantiate1Over :: ASetter s t (Bind () a) a -> a -> s -> t
instantiateOver  :: ASetter s t (Bind b a) a -> (b -> a) -> s -> t
closedOver       :: Traversal s t a b -> s -> Either (NonEmpty a) t
maybeClosedOver  :: Traversal s t a b -> s -> Maybe t
```

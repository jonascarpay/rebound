# rebound
[![rebound on hackage](https://img.shields.io/hackage/v/rebound)](http://hackage.haskell.org/package/rebound)
[![rebound on Stackage Nightly](https://stackage.org/package/rebound/badge/nightly)](https://stackage.org/nightly/package/rebound)

_Make [`bound`](https://github.com/ekmett/bound/) Succ more_

Tools for working with Bird/Paterson-style De Bruijn-indices.
Essentially just [`bound`](https://github.com/ekmett/bound/)'s [`Bound.Scope.Simple`](https://hackage.haskell.org/package/bound-2.0.3/docs/Bound-Scope-Simple.html) module, i.e. the non-monadic version, but with extra tools for working with traversals, and with no dependencies.

In particular, it adds 

```haskell
abstract1Over    :: Eq a => ASetter s t a (Bind () a) -> a -> s -> t
abstractOver     :: ASetter s t a (Bind b a) -> (a -> Maybe b) -> s -> t
instantiate1Over :: ASetter s t (Bind () a) a -> a -> s -> t
instantiateOver  :: ASetter s t (Bind b a) a -> (b -> a) -> s -> t
closedOver       :: Traversal s t a b -> s -> Either (NonEmpty a) t
maybeClosedOver  :: Traversal s t a b -> s -> Maybe t
```

This is useful, for example, when you have term- and type variables in the same expression data type.

### Motivation

I like `bound`'s API, but it's often annoying or impossible to massage your term representation into something that works.
Therefore I often start out with something simpler (i.e. traversal-based rather than monadic) while experimenting.
When it's time to optimize, I switch to a more efficient (non-quadratic) representation.

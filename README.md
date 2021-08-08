# rebound
[![rebound on hackage](https://img.shields.io/hackage/v/rebound)](http://hackage.haskell.org/package/rebound)
[![rebound on Stackage Nightly](https://stackage.org/package/rebound/badge/nightly)](https://stackage.org/nightly/package/rebound)

_Make [`bound`](https://github.com/ekmett/bound/) Succ more_

Tools for working with traversal-based Bird/Paterson-style De Bruijn-indices, with zero non-base dependencies.

Conceptually, this package is very similar to [`bound`](https://github.com/ekmett/bound/)'s [`Bound.Scope.Simple`](https://hackage.haskell.org/package/bound-2.0.3/docs/Bound-Scope-Simple.html) module, i.e. the non-monadic version, but with an API focused on general traversals.
The simplest case where this is useful is when you have multiple variables/binders in the same expression data type, but this API works with any representation you can generate the appropriate traversal for.
That means that it can also work with named representations, or a nested transformer stack like `bound` itself.

As a rule of thumb, if your expression forms a monad, use `bound`, if not, you might be interested in this package instead.

The main API consists of

```haskell
abstract1Over    :: Eq a => ASetter s t a (Bind () a) -> a -> s -> t
abstractOver     :: ASetter s t a (Bind b a) -> (a -> Maybe b) -> s -> t
instantiate1Over :: ASetter s t (Bind () a) a -> a -> s -> t
instantiateOver  :: ASetter s t (Bind b a) a -> (b -> a) -> s -> t
closedOver       :: Traversal s t a b -> s -> Either (NonEmpty a) t
maybeClosedOver  :: Traversal s t a b -> s -> Maybe t
```

where `Bind` is equivalent to `bound`'s `Var`.

All of the functions in the above API have non-`Over` versions that mirror `bound`, for working with `Functor` and `Traversable` instances.

## Examples

### Basic API

```haskell
λ> abstract1Over (_Left . traverse) 'x' (Right 9)
Right 9
λ> abstract1Over (_Left . traverse) 'x' (Left "xyz")
Left [Bound (),Free 'y',Free 'z']
```

### System F

```haskell
data Expr t v
  = Var v
  | Lam (Type t) (Expr t (Bind () v))
  | App (Expr t v) (Expr t v)
  | TyLam (Expr (Bind () t) v)
  | TyApp (Expr t v) (Type t)

data Type t
  = TVar t
  | TArr (Type t) (Type t)
  | TForall (Type (Bind () t))
  deriving (Functor)

traverseExpr ::
  Applicative m =>
  (t -> m t') ->
  (v -> m v') ->
  (Expr t v -> m (Expr t' v'))
traverseExpr ft fv = error "exercise for the reader"

types :: Traversal (Expr t v) (Expr t' v) t t'
types f = traverseExpr f pure

vars :: Traversal (Expr t v) (Expr t v') v v'
vars f = traverseExpr pure f

lam :: Eq v => v -> Type t -> Expr t v -> Expr t v
lam v t = Lam t . abstract1Over vars v

tyLam :: Eq t => t -> Expr t v -> Expr t v
tyLam t = TyLam . abstract1Over types t

forall :: Eq t => t -> Type t -> Type t
forall t = TForall . abstract1 t
```

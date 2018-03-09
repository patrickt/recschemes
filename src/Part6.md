Let's take a look at the various folds we've described. We'll use Kmett's `Base`-functor formulation.

``` haskell
cata  :: (Base t a -> a) -> t -> a
para  :: (Base t (t, a) -> a) -> t -> a
histo :: (Base t (Cofree (Base t) a) -> a) -> t -> a
```

Bear with me for a second. Let's rephrase `cata` in terms of the `Identity` functor, the simplest functor there is—all it does is wrap a given datum `a` in an `Identity a`.

```haskell
cata  :: (Base t (Identity a) -> a)        -> t -> a
para  :: (Base t (t, a) -> a)              -> t -> a
histo :: (Base t (Cofree (Base t) a) -> a) -> t -> a
```

The similarity in shape among these folds should be visible to you. They are all the same, save for the last parameter in the `Base` datum. This suggests that there's some way to generalize these three functions. But in order to do so, we need to have some sort of operation that encompasses operations on `Identity a`, `(t, a)`, and `Cofree (Base t) a`—what, in other words, do they have in common?

These three types are all comonads. Given some comonad `w` containing an `a`, we can use the `extract` function to get an `a` out of it:

``` haskell
extract :: Comonad w => w a -> a

extract (Identity "hello")       ==> "hello"
extract ("hello", "goodbye")     ==> "goodbye"
extract ("greetings" :< Nothing) ==> "greetings"
```

This is a clue! There exists some function, a *generalized catamorphism*, that, given a function operating on the `Identity` comonad yields `cata`; 
similarly, given a function over the tuple comonad `(, e)`, we should be able to yield `para`, and given something over `Cofree`, we will get `histo`. 

### Distributive Laws

```
Base t (w b) -> w (Base t b)
```

Given a `Base t` containing a comonad `w` of `b`, a distributive law tells us how to move—to *distribute*—that `w` out of the innards of that `Base t` and have it wrap the `Base` entirely.

TODO explain how this looks like distribution of math.

Let's remember the definition of `Identity`, then write a distributive law for `Identity` values.

``` haskell
newtype Identity a = Identity { runIdentity :: a } deriving Functor

distIdentity :: Functor f => f (Identity a) -> Identity (f a)
```

The functor instance for `f` gives us `fmap`, which operates on the contained value, and `runIdentity` gives us a way to extract that value. 

```
distIdentity =   fmap runIdentity -- yields an (f a)
             >>> Identity         -- yields an Identity (f a)
```

Okay, pretty straightforward. An additional bonus is that this function imposes no runtime cost: because `Identity` is a newtype, both its constructor `Identity` and its extraction function `runIdentity` will be optimized out by GHC.

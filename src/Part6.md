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

```
gcata dist walg = walg . extract . c where
	c = dist . fmap (duplicate . fmap walg . c) . project
```

```
cata alg = c where c = alg . fmap c . project
```

Like `cata`, `gcata` starts off by `project`ing its argument into a `Base` functor, then recursing, with `fmap` and the provided algebra, into the contents of that `Base` functor. Once we've reached the bottom of that `Base` functor, 
we begin to fold the structure leaf-to-root. At that point we apply the algebra `f`.  At this point, `cata` has nothing left to do, since the F-algebra has yielded us a final result. 

The same is not true with `gcata`—we have to do a few more gyrations to ensure we get an ordinary `a` value out of the ultimate result, rather than some result wrapped in the `w` comonad. 
What Kmett does here is extremely clever: he passes the result of applying the w-algebra to `duplicate`, which wraps the comonad `w` in another layer of `w`—an `Identity a` becomes an `Identity (Identity a)`, a tuple `(a, b)` becomes a tuple `(a, (a, b))`, and so on.
At this point we have a `Base t (w (w a))`.
At this point we apply the distributive law. That yields us a `w (Base t (w a))`.
Then we apply `extract` to discard the outer `w`, yielding us a `Base t (w a)`.
Since our w-algebra takes a `Base t (w a)`, we apply it one more time, finally yielding an `a`.

### Distributive Laws

```
Base t (w b) -> w (Base t b)
```

Given a `Base t` containing a comonad `w` of `b`, a distributive law tells us how to move—to *distribute*—that `w` out of the innards of that `Base t` and have it wrap the `Base` entirely.

TODO explain how this looks like distribution of math.

Let's remember the definition of `Identity`, then write a distributive law for `Identity` values.

``` haskell
newtype Identity a = Identity { runIdentity :: a } deriving Functor

distIdentity :: Functor (Base t) => Base t (Identity a) -> Identity (Base t a)
```

The functor instance for `Base t` gives us `fmap`, which operates on the contained value, and `runIdentity` gives us a way to extract that value. 

```
distIdentity =   fmap runIdentity -- given a 'Base t (Identity a)', yields a 'Base t a'
             >>> Identity         -- given a 'Base t a', yields an 'Identity (Base t a)'
```

Indeed, because `distIdentity` depends only on the `Functor` instance for `Base`, we can generalize the type signature to any `Functor` f:

```haskell
distIdentity :: Functor f => f (Identity a) -> Identity (f a)
```

Okay, pretty straightforward. An additional bonus is that this function imposes no runtime cost: because `Identity` is a newtype, both its constructor `Identity` and its extraction function `runIdentity` will be optimized out by GHC.

The distributive law for `para`, over the tuple comonad, is a little bit more complicated, but not overly so. It depends on an implementation of `embed` for `t` contained in the `Base` functor, which means we need a `Corecursive` instance.

```
distTuple :: Corecursive t => Base t (t, a) -> (t, Base t a)
```

Given a `Base t` containing a tuple `(t, a)`, we have to pull that contained `t` out while preserving the `a` contained therein. And because we don't know anything about the shape of the `Base` functor with which we're provided, we can't pattern-match. 
However, the `Corecursive` instance gives us `embed`, and because `Corecursive` implies `Functor`, we can `fmap` onto the contents of that `Base`. Along with the built in `fst` and `snd` functions, we can do it too:

```
distTuple b = (justT, baseA) 
	where baseT = fmap fst b  -- given a 'Base t (t, a)', yields a 'Base t t'
	      justT = embed baseT -- given a 'Base t t', yields a t
		  baseA = fmap snd b  -- given a 'Base t (t, a)', yields a 'Base t a'
```

Finally, we need one for `Cofree`. 

``` haskell
distHisto :: Functor f => f (Cofree f a) -> Cofree f (f a)
```

The implementation of this one is a little bit subtle, so I don't expect you to follow every step, but the same principles apply: given an `f` containing a `Cofree`, we distribute the `f` inside the `Cofree` and pull said `Cofree` out to the top level.

``` haskell
distHisto = Cofree.unfold cofreeToTuple where
	cofreeToTuple f = (fmap extract f, fmap Cofree.unwrap f)
```

Rather than diving into the rich details of `Cofree`, for our purposes it'll be good enough to say that `unfold`, given a seed, generates a `Cofree` out of a function that returns, at each stage of the unfold, a value and some further seed with which to continue. Remember that `Cofree` is analogous to an infintely-nested tuple; we can't write an infinitely-nested tuple in Haskell, but we can hand a tuple of value and seed to `unfold` and it will take care of creating one for us.[^1]

[^1]: I usually try to provide explicit types for all my helper functions, but `cofreeToTuple`'s is a touch intimidating: `f (Cofree f a) -> (f a, f (f (Cofree f a)))`.

In these entries I've tried to emphasize how useful recursion schemes
are in a real-world context. The subject under discussion
today—Uustalu, Tarmo, and Vene's [Recursion Schemes from
Comonads][rsfc]—is, I have to admit, not flush with potential
real-world applications. However, it is very beautiful, and beauty is
a worthy goal in itself.

Let's take a look at the various folds we've described thus far.

``` haskell
cata  :: (f a -> a)            -> Fix f -> a
para  :: (f (Fix f, a) -> a)   -> Fix f -> a
histo :: (f (Cofree f a) -> a) -> Fix f -> a
```

These all have the same fundamental shape: they take some sort of
algebra as their first (an F-algebra for `cata`, an R-algebra for
`para`, a CV-algebra for `histo`), the fixed point of a functor
on which to operate as their second argument, and return the
result of collapsing that functor with the provided algebra.

Bear with me for a second. Let's rephrase `cata` in terms of the `Identity` functor.

```haskell
cata  :: (f (Identity a) -> a) -> Fix f -> a
para  :: (f (Fix f, a) -> a)   -> Fix f -> a
histo :: (f (Cofree f a) -> a) -> Fix f -> a
```

The similarity in shape among these folds should be visible to you.
**This is a hint.** The fact that all these fold functions accept a
`Base t` over some collection type—an `Identity`, a tuple, a
`Cofree`—and return a function of `t -> a`, is a sign that there's
some generality that we're not taking advantage of.


They are all the same, save for the last parameter in the `Base`
datum. This suggests that there's some way to generalize these three
functions. But in order to do so, we need to have some sort of
operation that encompasses operations on `Identity a`, `(t, a)`, and
`Cofree (Base t) a`—what, in other words, do they have in common?
Well, they're all comonads.

## Comonad

Much ink has been spilled on the subject of comonads and their myriad uses. Dave Liang's post is a fun overview of what they can be used for, and Bartosz Milewski's explanation of the theory behind them is, as is so often the case, essential reading.

For the purposes of this post, however, we don't need to go deeply into the theory behind them. All we need to know is that, like its dual the monad, a comonad supports two fundamental operations, `extract` and `duplicate`.

A monad `m` supports wrapping a given datum `a`, yielding an `m a`, with the `return` function. Correspondingly, a comonad `w` supports the opposite of that—given a `w a`, we can get that `a` out of it with `extract.

```haskell
return  :: Monad m => m -> m a

extract :: Comonad w => w a -> a
```


``` haskell
extract (Identity "hello")       ==> "hello"
extract ("hello", "goodbye")     ==> "goodbye"
extract ("greetings" :< Nothing) ==> "greetings"
```

The other fundamental monad operation is `join`: given a monad holding monadic values, `m (m a)`, we can "squash" or "flatten" the contained `m a` values into a single `m a`.

```haskell
join :: Monad m => m (m a) -> m a
```

Correspondingly, comonads support the dual instruction: given a `w a`, we can "project" or "duplicate" it into a `w (w a)`.

``` haskell
duplicate (Identity "hello")     ==> (Identity (Identity "hello"))
duplicate ("hello", "goodbye")   ==> ("hello", ("hello", "goodbye"))
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

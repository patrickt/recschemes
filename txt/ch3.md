On the [last episode][previous] of this exploration of recursion schemes, we defined the catamorphism, our first generalized fold operation over any recursive data structure. Catamorphisms admit a beautiful definition and are useful in hosts of situations. But they have their limits: in this installment, we'll explore these limits, and we'll discuss how to get around them by introducing a more powerful construct—the paramorphism.

## A Refresher

In the past two posts, we defined a datatype `Term` that represents the fixed-point of a functor `f`, with an `In` constructor that ties an `f (Term f)` into a `Term f`, and an `out` deconstructor that unties a `Term f` into an `f (Term f)`:

```haskell
newtype Term f = In { out :: f (Term f) }
```

Using `fmap`, and the property that `fmap` is the identity function over a `Functor` with no children, we can define a `bottomUp` function that applies a type-preserving transformation to any `Term f`:

```haskell
bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
  out                    -- 1) unpack
  >>> fmap (bottomUp fn) -- 2) recurse
  >>> In                 -- 3) repack
  >>> fn                 -- 4) apply
```

And, when we omit the repacking stage in the above definition, we yield `cata`, a generalized fold operator that allows us to collapse a given `Term f` into an accumulated value `a`, using an `Algebra` that reunites an `f a` into an `a`:

```haskell
type Algebra f a = f a -> a

cata :: (Functor f) => Algebra f a -> Term f -> a
cata fn =
    out                -- 1) unpack
    >>> fmap (cata fn) -- 2) recurse
    >>> fn             -- 3) apply
```

## Paramorphisms

Catamorphisms are simple and elegant, but in many real-world use-cases they are insufficient. For example, though the function we pass to `cata` allows us to view the data we're transformating, it loses information about the original structure: in the case of pretty-printing, we only have access to the currently-printed tree (the `f Doc` for any fix): any information about the structure of the original `Term Expr` is lost, as it has already been pretty-printed.

This would be a problem if you wanted to print, say, zero-argument functions in a different manner than other functions: you'd have to use `==` to examine the `Doc` type, then dispatch on the result to implement this different behavior. And your `Doc` type may omit an `Eq` instance, so `==` may not even be possible! (Besides, looking at the pretty-printed results to infer their inputs is clumsy at best and often inaccurate in the presence of hyphenation or spacing.)

It would be ideal if our `Algebra` could, when examining an `Expr` node, have access both to its pretty-printed representation and its original representation as a `Term`—an `Algebra` that has access to the original, un-transformed datum, represented as its fixed-point `Term`. That is to say, whereas `Algebra` is a function from a container `f` of `a`, we want a function from container that holds _both_ a `Term f` and its corresponding `a`. Rather than using a function with two arguments, we'll bundle these two arguments together in a tuple (for reasons that will become clear later).

```haskell
f (Term f, a) -> a
```

Algebras that carry this extra information are known as *R-algebras*.

```haskell
type RAlgebra f a = f (Term f, a) -> a
```

There is another type of morphism that allows you to traverse a structure with an R-algebra: the *paramorphism*. As with previous examples, I'm going to explicate the etymology in the hope that it slightly illuminates a complicated concept: the _para_ in paramorphism is the same as in _parallel_—from the Greek παρά, meaning "beside", "next to", or "alongside"[^para]. A paramorphism is like a catamorphism except that it can view the original structure _beside_ the structure that is being transformed.

So, let's implement paramorphisms. They'll look like the catamorphism we already discussed. But instead of just directly recursing into the structure with `fmap para`, we have to recurse with a function that returns a tuple, one that contains both the `Term` we're recursing into and the result of recursively applying `para`. We do this here with a helper function called `fanout`, which takes a `Term f` and returns both items in which we are interested: the `Term` itself, and the result of recursively applying `para`.

```haskell
para :: (Functor f) => RAlgebra f a -> Term f -> a
para rAlg = out >>> fmap fanout >>> rAlg
    where fanout :: Term f -> (Term f, a)
          fanout t = (t, para rAlg t)
```

And we're done! This is the classical definition of a paramorphism. With it, we can have our cake and eat it too: we can intelligently fold over a data structure without losing any information about the original representation of said structure.

And Haskell comes with a function that makes expressing the above `fanout` function even more elegant than it already is. The `&&&` combinator, provided in the `Control.Arrow` module, takes two functions[^fanout] `foo` and `bar` and returns another function that, when given a value `a`, returns a tuple consisting of `(foo a, bar a)`. Simply put, it combines the output of two functions.

Our `fanout` function, when provided with a `Term f`, needs to do two things: preserve its input in the first element of the tuple, and recurse with `para` into its input, providing the result of doing so in the second element. We can use `&&&` to express this concisely: given a `Term f`, we preserve the element with the identity function `id`, and apply `para` recursively to the argument. Let's express this as such:

```haskell
fanout = id &&& para f
```

Now we can express `para` with our new, beautiful `fanout` function:

```haskell
para' :: Functor f => RAlgebra' f a -> Term f -> a
para' f = out >>> fmap (id &&& para f) >>> f
```

The type signatures indicate that both these formulations of paramorphisms are equivalent: which one you choose is entirely up to which one you find more aesthetically pleasing.

## Obsoleting the Catamorphism

For extra credit: rather than using a function that takes a container of tuples (which can strike the eye as somewhat ugly), we can use one that takes two arguments, both the `Term f` and the container `f a`. (We can convert between this representation and `RAlgebra` trivially).

```haskell
type RAlgebra' f a = Term f -> f a -> a
```

Balazs Komuves refers to this formulation as "slightly less natural" in his [Fixplate](fixplate) library. The implementation is indeed less pleasing, as it cannot easily be expressed in a point-free fashion, but it has a nice property that we'll explore below.

```haskell
-- The & function is reverse function application,
-- just like the $ operator, but with its arguments flipped.
para'' :: Functor f => RAlgebra' f a -> Term f -> a
para'' alg t = out t & fmap (para'' alg) & alg t
```

And just as we were able to represent `bottomUp` in terms of `cata`, we can express `cata` in terms of `para'`—after all, _a catamorphism is merely a paramorphism that ignores the provided `Term`_. And Haskell provides the `const` function (aka the K-combinator) for just these situations where we want to ignore an argument to a function:

```haskell
cata' :: Functor f => Algebra f a -> Term f -> a
cata' = para'' (const f)
```

Beautiful, no? This is one of the really appealing things about recursion schemes: as we explore more and more powerful constructs, we see how the less-powerful constructs can be implemented straightforwardly in terms of more general ones.

## Exempli Gratia

The identity function `id`, by definition, returns its argument unchanged: `id(x)` can be replaced with `x` in every case. Let's imagine a pretty-printer that, for some reason[^reason], performs this optimization step on its output.

To do this with a simple catamorphism, we'd need to check every function-call's pretty-printed name to determine whether it is `id`, then return the argument unchanged—and, as I mentioned above, our pretty-printed `Doc` representation shouldn't even support an equality operation, so examining it is a no-go. However, we can do this easily with a paramorphism. In order to avoid having to write a bunch of tuples, I'm going to use the second representation of R-algebras above (the ternary function), and I'm going to use the `Expr` syntax tree defined in previous installments.

```haskell
{#- LANGUAGE RecordWildCards -#}
import qualified Text.PrettyPrint.ANSI.Leijen as P

fastPretty :: RAlgebra' Expr Doc

-- All our cases, aside from the `Call` nodes in which
-- we are interested, are the same as in the pretty-printing
-- catamorphism in the previous installment. We just ignore
-- the first `Term` argument because it doesn't have anything we need
-- to look at.
fastPretty _ (Literal i) = P.int i
fastPretty _ (Ident s) = P.text
-- uninteresting cases omitted, blah blah blah

-- Here's where it gets interesting. We're going to look
-- at the first argument to determine  whether this is a
-- `Call` node with the function name (an `Ident`) named `id`.
-- If so, we'll just return the only argument provided.
fastPretty (In (Call { name = "id", ..})
           (Call {args = [theArg], ..}) = theArg

-- Otherwise, we won't look at the first `Term` argument,
-- and just glom the name and the parenthesized and
-- comma-separated arguments together.
fastPretty _ (Call f args) = f <> P.parens ("," `P.punctuate` args)
```

During complicated tree transformations, the context of the structure you're transforming will eventually come into play. Catamorphisms don't let you examine this context, but paramorphisms do.

## Apomorphisms

In the previous post, we defined `ana`, the anamorphism, a generalized unfold operating on any given data type to generate a `Term f`. While unfolds are a little more abstruse and less common than folds, it's worth walking through their construction, if only to observe the generality achieved from reversing the arrows in a given morphism.

We expressed `ana` as the dual to `cata`, replacing instances of `out` with `In`, and replacing left-to-right function composition with the right-to-left equivalent, `<<<` (more commonly expressed with Haskell's `.` function)—in short, reversing the arrows of the definition.

```haskell
cata f = out >>> fmap (cata f) >>> f

ana f = In <<< fmap (ana f) <<< f
```

And we defined the function argument that `ana` takes as a `Coalgebra`, seeing as how it is dual to the `Algebra` we already defined:

```haskell
type Coalgebra f a = a -> f a

ana f :: (Functor f) => Coalgebra f a -> a -> Term f
```

It stands to reason that we can define the dual of a paramorphism—a co-paramorphism. But, as always, we have a better name for this: the dual of a paramorphism is an _apomorphism_. Just as the ana- prefix is the opposite of the cata- prefix, so the para- prefix is the opposite of the apo- prefix. In this case, apo- comes from the Greek ἀπο, meaning "away from" or "separate", as in "apogee" (the moon being away from the earth) or "apostasy" (someone turning away from their beliefs).

So, let's start by defining the categorical dual of the R-algebra. We've reversed the arrows in every case, so the following definition should be correct, right?

```haskell
type Nope = a -> f (Term f, a)
```

Wrong! We have to apply the dual to every construct in the definition of `RAlgebra`. We need to reverse the direction of the function, yes, but we also need to reverse the tuple associated with the above definition. So what's the dual of a tuple?

Well, let's consider what a tuple is for. Given two arguments `big` and `pac`, a tuple bundles both of them together as `(big, pac)`. That makes sense, yes, but what can we do with both of these arguments that fits the notion of the "opposite" of holding both? Well, we can hold one or the other. And Haskell provides a concept to hold either a `big` or a `pac`: namely, `Either`. So, given that an `Algebra f a` holds a `Term f` and an `a`, we can express the dual of an R-algebra using an `Either`:

```haskell
type RCoalgebra = a -> f (Either (Term f) (f a))
```

But what does this _mean_ when we're using apomorphisms in practice? Well, it allows us to _separate_ the flow of computation during our unfolds. If our R-coalgebra returns a `Left` value in which is contained a `Term`, the apomorphism will terminate and return the provided value. If it returns a `Right` value containing an `f a`, the unfold will continue onwards. This is cool! The ability to terminate during a corecursive iteration depending on the argument is a very useful property—and we need no imperative constructs such as `break` or (_shudder_) exceptions[^except].

So, just as we expressed `ana` by reversing the arrows of `cata`, we can express `apo` by reversing the arrows of `para`:

```haskell
para :: Functor f => RAlgebra' f a -> Term f -> a
para f = out >>> fmap fanout >>> f where fanout = id &&& para f

apo f :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In <<< fmap fanin <<< f where fanin = ???
```

It may not be immediately obvious how to implement `fanin`. But, when you reverse the arrows of the `fanout` definition above (I have omitted said reversal for brevity's sake), you'll discover that you yield a function that takes an `Either (Term f) a` and returns a `Term f`.

```haskell
fanin :: Either (Term f) a -> Term f
```

As such, our function will handle this either by applying `id` in the case of a `Left` value (as getting a `Term` means that we can just return the `Term`) and recursing with `apo` in the case of a plain old `a` value, out of which we ultimately yield a `Term`, thanks to the ultimate signature of `apo`. And Haskell's built-in `either` function, which takes two functions and an Either and returns a result of applying the first to a `Left` case or the second to the `Right` case, allows us to express this `fanin` function beautifully. `id` does nothing the value contained inside a `Left`, returning just a `Term f`, and `apo f` continues the unfold operation when provided a `Right`:


```haskell
apo f :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In <<< fmap fanin <<< f where fanin = either id (apo f)
```

Similarly, we can rewrite `fanin` with `|||`, the dual of the `&&&` function above. (The operators here are a useful visual mnemonic: `&&&` uses both the functions it provides, where as `|||` uses one or the other).

```haskell
apo f :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In <<< fmap (id ||| apo f) <<< f
```

## That's All, Folks

If you made it this far, you've got some serious chops, and I salute you. Next time, we'll look at futumorphisms and histomorphisms, and uncover some seriously powerful constructs (and some seriously dubious etymologies).

I am indebted to Rob Rix, Colin Barrett, and Manuel Chakravarty for their input and suggestions regarding this post.

_In part four, we explore [histomorphisms and futumorphisms](https://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/)._

[^para]: Modern English tends to use "para" as a prefix meaning "pseudo" or "abnormal" (as in "parapsychology" or "paresthesia")—this is an extension of the "alongside" meaning, implying that abnormal things appear alongside normal things. Be sure not to confuse these two meanings—there's nothing abnormal or second-class about paramorphisms.

[^fanout]: Technically, two `Categories`, but if you use instances of `Category` beyond `(->)` then you are way ahead of me.

[^reason]: You'd have to be a complete lunatic to put this optimization step in your pretty-printer—you'd either perform this as an optimization over the original code or during a conversion to a separate intermediate representation—but I'm going to stick with this incredibly contrived example, because the `Doc` type makes it very clear, when operating on `Expr` types, when and where the pretty-printing step is happening.

[^except]: I hate exceptions. Hate 'em. But that subject deserves its own post.

[previous]: http://blog.sumtypeofway.com/recursion-schemes-part-2/

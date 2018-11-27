On the [last episode][previous] of this very-infrequently-updated analysis of _[Programming with Bananas, Lenses, Envelopes, and Barbed Wire][bananas]_, we took a look at how to represent and traverse nested structures. This time, we'll start getting into the meat of the paper[^meat]. We'll define four simple recursion schemes, explore how they relate to each other, and I'll offer up some example sitations (both real-world and contrived) of situations that admit the use of recursion schemes.

We'll start by defining a simple syntax tree, as we did [last time][previous].

```haskell
{-# LANGUAGE DeriveFunctor #-}

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String  }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)
```

We define two root constructors to represent the leaf nodes of the tree (`Literal` and `Ident`, wrapping `Int` and `String` values respectively) and five other constructors. Every position in which subexpressions may appear -- the left-hand and right-hand sides to a binary operation, the target of a unary operation, the function and arguments in a function invocation -- is represented by a value of type `a`, in terms of which Expr is defined[^kind].

We use the `DeriveFunctor` GHC extension to provide a definition of the `Functor` typeclass for `Expr`, which provides us with an `fmap` function. `fmap f`, applied to a given `Expr`, applies `f` to each subexpression `a`, and is the identity function when passed a `Literal` or `Ident` value, as they contain no subexpressions.

At this point, we can represent expressions with no subexpressions with the type `Expr ()`; for expressions at most one level of subexpressions, we can use `Expr (Expr ())`; for two, `Expr (Expr (Expr ())`, and so on and so forth. What we want is to represent an arbitrarily-nested `Expr` -- in essence, an `Expr (Expr (Expr (Expr ...)))` -- but, since Haskell doesn't support infinite types, we have to resort to a least-fixed-point combinator. We call this combinator `Term`, with an `In` constructor and an `out` accessor function.

```haskell
newtype Term f = In { out :: f (Term f) }
```

Now we represent arbitrarily-nestable `Expr`s with values of type `Term Expr`, obtained by applying the `In` constructor with a constructor from `Expr`[^noise]:

```haskell
ten, add, call :: Term Expr
ten  = In (Literal { intVal = 10 })
add  = In (Ident { name = "add" })
call = In (Call { func = add, args = [ten, ten]}) -- add(10, 10)
```

Using the `>>>` operator for left-to-right function composition, we expressed a generalized bottom-up traversal capable of operating on any data type that implements the [`Functor`][functors] typeclass.

```haskell
bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
  out                    -- 1) unpack a `Term a` into an `a (Term a)`
  >>> fmap (bottomUp fn) -- 2) recurse, with fn, into the subterms
  >>> In                 -- 3) repack the `a (Term a)` into a `Term a`
  >>> fn                 -- 4) finally, apply fn to the packed `Term a`
```

While this is a pleasing and concise representation of bottom-up transformations, it's not as powerful as it could be: specifically, `fn` is limited to taking _and returning_ a value of type `Term f`. We could not use `bottomUp` to count the total number of subexpressions in a tree (going from `Expr`s to `Int`s), nor could we transform this tree to a DOM representation (`Node`), nor could we render a term into a pretty-printed representation (`Doc`).

This is a direct result of the third step above: after recursing into the data structure with `fn` in step #2, we stuff it into a `Term` using the `In` constructor in step #3 before passing it to `fn`. This forces `fn` to both take and return a `Term`. What if we wrote a version of `bottomUp` that omitted that particular call to `In`? Does it typecheck?

You're damn right it does:

```haskell
mystery fn =
  out                   -- 1) unpack the Term
  >>> fmap (mystery fn) -- 2) recursively apply `fn`
  >>> fn                -- 3) apply `fn`
```

Loading this function in `ghci` and querying its type yields the following result:

```haskell
λ> :t mystery
mystery :: Functor f => (f a -> a) -> Term f -> a
```

Cool. How does this work?

## Algebras

Let's take a closer look at that first parameter:

```haskell
Functor f => (f a -> a)
```

This is a function type, taking as input a container[^functor] `f` of values of type `a` and returning a bare value of type `a`. If we wanted to write a `countNodes` function that counts (as an `Int`) the number of subexpressions within a given `Term Expr`, `f` would be equal to `Expr` (which has a `Functor` instance), and `a` would be `Int`.

```haskell
countNodes :: Expr Int -> Int
```

The crucial element of understanding this function is its first parameter, here `Expr Int`. It captures an `Expr` _in the process of transformation_ -- because subexpressions (the `lhs` and `rhs` field of `Binary`, the `func` and `args` fields of `Call`) were represented as values of the parameterized type `a`, we can capture the bottom-up nature of transformation _in the type itself_. This means that each case for `countNodes` is easy to express: merely add 1 to the sum of all the contained subexpressions.

```haskell
countNodes (Unary _ arg)         = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn args)        = fn + sum args + 1
countNodes (Index it idx)        = it + idx + 1
countNodes (Paren arg)           = arg + 1
```

And our base case for this transformation falls out easily: `Literal` and `Ident` have no subexpressions, so they just return 1:

```haskell
countNodes (Literal _) = 1
countNodes (Ident   _) = 1
```

Applying `mystery countNodes` to our example `add(10, 10)` datum should yield 4 (one for the identifier `add`, one for the function call, and two for the . Does it?

```haskell
λ> mystery countNodes call
4
```

Dope.

I had a major mental block in understanding when, in fact, the `countNodes` function was actually called, and how `mystery` managed to recurse as deeply as possible into the passed structure. The key lies in the invocation of `fmap` within `mystery`:

```haskell
fmap (mystery fn)
```

Because `fmap f` applies `f` to each subexpression within an `Expr`, `mystery` starts out by recursing as deeply as possible into the `Term` it is passed, since it calls itself recursively with `fmap`. It's almost magical how `mystery` "knows" how to stop recursing -- but it lies in the definition of `fmap`. `fmap mystery` _is the identity function_ over `Literal` and `Ident` values, as they do not contain any subexpressions. At this point, `mystery` stops recursing, applies the function `f`, and returns into its previous invocations. As the stack unwinds, `fn` gets applied to the next-highest node in the tree, then the next, then the next, until all the original `Expr` values are gone and we yield only an `a`. It all comes down to the capabilities of `fmap` -- from a straightforward purpose and declaration emerges a deep and subtle way to fold over a structure.

Indeed, functions of type `f a -> a` are so ubiquitous that we refer to them by their own name:

```haskell
type Algebra f a = f a -> a
```

When you see a value of type `Algebra f a`, know that it's a function from a container `f` to a collapsed value `a`. We could rewrite the type signature of the above `countNodes` function to use it:

```haskell
countNodes :: Algebra Expr Int
```

The use of the term "algebra" to in this context may seen a bit discomfiting. Most people use the term "algebra" to describe manipulating numerical expressions (as well as the associated drudgery of childhood math courses). Why are we overloading this term to represent a class of functions?

The etymology of "algebra" can shed some light on this—it stems from the Arabic root جبر, _jabr_, which means "restoration" or "reunion". An algebra as a function that *reunites* a container of `a`'s—an `f a`—into a single accumulated `a` value.

### Catamorphisms

So, now that we have a grasp on what an algebra is and why we're calling it that, let's rewrite the type signature of `mystery` using the `Algebra` type synonym.

```haskell
mystery :: (Functor f) => Algebra f a -> Term f -> a
```

This `mystery` function is known as a *catamorphism*, usually given the name `cata`:

```haskell
cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f
```

Again, though the term "catamorphism" may seem needlessly arcane, its etymology both simplifies and clarifies its purpose: the "cata" in "catamorphism" is the same "cata" in "catastrophe", "catabolism", and "catalyst"—from the Greek κατα, meaning "downwards", "into", or "collapse". Just as a catastophe is an unlucky event that tears things down, and catabolism is the collapse of muscle fibers, a catamorphism uses an algebra (a reunion) to collapse a container of values into a single value.

If you're reminded of the `fold` operation on lists, you're on the right track: a list fold (specifically `foldr`[^folds]) is merely a catamorphism limited to operate on lists (the `[]` type, the archetypal `Functor`). That's the essence of catamorphisms: they're *generalized fold operations*, applicable to *any* functor—not just lists—all without having to write a line of boilerplate traversals and without sacrificing a mote of type safety.

Another excellent example of catamorphisms is pretty-printers for trees. If the representation you require of a tree type can be expressed in a purely bottom-up manner -- that is to say, it requires no information about the original structure of the tree being transformed -- you can express pretty-printing functions in a truly wonderfully concise manner. (Spoiler alert: later on, we'll figure out how to do this even when you need access to the original structure.) We can describe a pretty-printing algebra from `Expr` to `Doc`, an abstract document type provided by the [Text.PrettyPrint][pp] module.

```haskell
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

prettyPrint :: Algebra Expr Doc
```

Again, expanding the type synonym yields us a function type, going from `Expr`s with values of type `Doc` as their subexpressions, out to a final `Doc` value. These values represent the leaves of the nodes that have already been pretty-printed.

```haskell
prettyPrint :: Expr Doc -> Doc
```

Our base cases are straightforward:

```haskell
prettyPrint (Literal i) = P.int i
prettyPrint (Ident s) = P.text s
```

And our inductively-defined cases are beautifully clutter-free:

```haskell
prettyPrint (Call f as)     = f <> P.parens (P.punctuate "," as)   -- f(a,b...)
prettyPrint (Index it idx)  = it <> P.brackets idx                 -- a[b]
prettyPrint (Unary op it)   = (P.text op) <> it                    -- op x
prettyPrint (Binary l op r) = l <> (P.text op) <> r                -- lhs op rhs
prettyPrint (Paren exp)     = P.parens exp                         -- (op)
```

Applying `prettyPrint` to our example `call` datum should yield a nice representation:

```haskell
λ> cata prettyPrint call
add(10,10)
```

One final detail: we can represent our `bottomUp` function in terms of `cata`. Indeed, `bottomUp f` is just `cata f`, with the additional step of stuffing the accumulator value into a `Term` with `In` before handing it off to `f`:

```haskell
bottomUp f = cata (In >>> f)
```

### Laws

This formulation of recursive traversals sure is elegant -- but elegance isn't enough. THe real reason to use catamorphisms is that every catamorphism obeys a set of laws.[^cata]

#### The Identity Law

The simplest law is the identity law:

```haskell
cata In = id
```

This is pretty straightforward. The `In` constructor, which provides the fixed-point of a functor, is the simplest algebra: given a functor `f` wrapping a `Term f`, it turns it into a `Term f`, leaving the value untouched. And iterating over the contents of a structure without changing them is clearly the identity function.

Catamorphisms can also *fuse*.

```haskell
given alg :: f a -> a
and func  :: f a -> f
cata (alg >>> fmap func) =>
   (cata alg) >>> func
```

This is an extraordinarily powerful property of catamorphisms: in the above example, it allows you to avoid successive invocations of fmap.

Catamorphisms also *compose*: given an algebra over a given type

```haskell
-- given alg  :: f a -> a
-- and   func :: f a -> g a
cata (f >>> In) >>> cata g
   ==> cata (f >>> g)
```

That is to say, the result of composing two separate catamorphisms is equivalent to a single catamorphism using the combination of the contained algebras. This is amazing, almost magically so: the composition law allows us to turn two separate calls to `cata` into one.

### Was That Really So Hard?

Thanks to Colin Barrett and Rob Rix for reading drafts of this essay, and thanks to Rob for pushing me to publish it. I'm sorry it took me so long.

On a personal note, I am deeply grateful for all the comments I've received thus far, both complimentary and constructive.

_In part 3, we explore the limits of catamorphisms, and address these limits with [paramorphisms and apomorphisms](http://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/)._

[^meat]: By "meat of the paper", I mean "the first two pages". Have patience.

[^kind]: In other words, `Expr` is an inductively-defined data type of kind `* -> *`.

[^noise]: Packages like [compdata][compdata] provide Template Haskell magic to avoid the syntactic clutter associated with applying the `In` constructor everywhere: given our above definition of `Expr`, we could use the `smartConstructors` splice to generate `iLiteral`, `iIdent`, `iUnary`, etc. functions, each of which are straightforward compositions of the constructors of `Expr` with the `In` fixed-point operator.

[^functor]: The purists in the audience are no doubt cringing at my use of "container" to describe the capabilities of a `Functor`. As the Typeclassopedia points out, `Functor`s are broader than just containing values: the best phrase for a `Functor f => f a` is "a value `a` within some computational context `f`. That doesn't exactly roll off the tongue, though, so I will be using "container" throughout. Sorry, purists. (But I already lost you with my hand-wavy treatment of least-fixed-points and codata last time.)

[^cata]: This is assuming that we're working in a purely functional language (and that nobody's calling out to `unsafePerformIO`).

[^folds]: Digression: Some of you may be wondering "if the catamorphism applied to lists is equivalent to `foldr`, what's the analogous construct for `foldl`?" This is somewhat of a trick question, as `foldl` can be expressed in terms of `foldr`, as [Oleg demonstrates][oleg-folds].

[bananas]: http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf
[previous]: http://patrickthomson.ghost.io/an-introduction-to-recursion-schemes/
[pretty]: https://hackage.haskell.org/package/pretty-1.1.1.0/docs/Text-PrettyPrint-HughesPJ.html
[functors]: http://en.wikibooks.org/wiki/Haskell/The_Functor_class
[compdata]: https://hackage.haskell.org/package/compdata
[pp]: https://hackage.haskell.org/package/pretty-1.1.1.0/docs/Text-PrettyPrint.html
[oleg-folds]: http://okmij.org/ftp/Haskell/AlgorithmsH1.html#foldl

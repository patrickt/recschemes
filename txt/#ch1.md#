In 1991, Erik Meijer, Maarten Fokkinga, and Ross Paterson published their now-classic paper [*Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire*][bananas]. Though this paper isn't widely known outside of the functional programming community, its contributions are astonishing: the authors use category theory to express a set of simple, composable combinators, called *recursion schemes*, that automate the process of traversing and recursing through nested data structures. Though recursion schemes predate Meijer et. al's work, this paper brings the enormous abstractive power of category theory to bear on the subject of traversing data structures—it's a magnificent example of how category-theoretical concepts can bring both rigor and simplicity to day-to-day programming tasks.

[bananas]: http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf

Because nested structures appear in almost every problem domain and programming environment, from databases to 3D graphics to filesystems, the act of iterating through these structures is common, so common that most programmers barely notice when they're doing it. As such, generalizing the act of recursive traversals provides immediate real-world benefits: our new generalized traversal can replace a host of type-specific traversal functions. In addition, by decoupling *how* a function recurses over data from *what* the function actually does, we reduce cognitive overhead and can focus entirely on the core behavior of our recursive functions. No matter the structures in question—lists, directory hierarchies, control flow graphs, database records—recursion schemes bring us an orderly and predictable way to traverse them. In addition, recursion schemes aren't a product of any one programming language or environment—you can express recursion schemes in any language with first-class functions. Clojure, for example, uses them to power its [clojure.walk][walk] API for generically traversing s-expressions and maps.

[walk]: http://richhickey.github.io/clojure/clojure.walk-api.html

Meijer et. al go so far as to condemn functional programming without recursion schemes as morally equivalent to imperative programming with `goto`. While comparisons to Djikstra's [infamous letter to the ACM][goto] are often inane, the analogy is apt: just as using `while` and `for` loops rather than `goto` brings structure and harmony to imperative control flow, the use of recursion schemes over hand-written  brings similar structure to recursive computations. This insight is so important that I'll repeat it: *recursion schemes are just as essential to idiomatic functional programming as `for` and `while` are to idiomatic imperative programming*.

[goto]: http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html

I've chosen to express the ideas in *Bananas, Lenses, Envelopes and Barbed Wire* in Haskell, though the paper was written years before Haskell came to prominence[^notation]. If you don't know Haskell very well, **don't panic**: you don't need to be a Haskell whiz to understand the ideas presented here. I assume only a basic familiarity with [Haskell syntax][syntax] and the use of [algebraic data types][adts]. I'm going to rely on a few idioms to better illustrate the concepts underlying recursion schemes—when I do, I will explain what happens behind the scenes. If you're wholly unfamiliar with Haskell, you may want to dip into the first few chapters of [Learn You a Haskell][lyah].

[syntax]: http://cheatsheet.codeslower.com/CheatSheet.pdf
[adts]: http://learnyouahaskell.com/making-our-own-types-and-typeclasses
[lyah]: http://learnyouahaskell.com


I'll start with the simplest way to represent a well-typed syntax tree, then show how that simplicity makes it difficult to write a function that generically traverses and modifies trees. I'll then redefine our syntax tree so as to take advantage of existing Haskell idioms and the expressive power of parameterized data types. Finally, I'll show how recursion schemes emerge naturally when we express step-by-step descriptions of recursion patterns with common Haskell idioms.

## Syntax Trees and Recursion

Let's take a look at the simplest way to represent a syntax tree in Haskell: an ordinary algebraic datatype.

```haskell
data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

data Expr
  = Index Expr Expr
  | Call Expr [Expr]
  | Unary String Expr
  | Binary Expr String Expr
  | Paren Expr
  | Literal Lit
  deriving (Show, Eq)

data Stmt
  = Break
  | Continue
  | Empty
  | IfElse Expr [Stmt] [Stmt]
  | Return (Maybe Expr)
  | While Expr [Stmt]
  | Expression Expr
  deriving (Show, Eq)
```


This is a perfectly adequate syntax tree: it's simple, straightforward, and works nicely with parsing libraries such as [attoparsec](http://hackage.haskell.org/package/attoparsec) or [Peggy](http://tanakh.github.io/Peggy/). Yet writing a function that operates on a `Expr` node and all its subexpressions is a tedious exercise indeed: here's an example that flattens an `Expr`, recursively removing all `Paren` nodes:

```haskell
-- this would turn the expression
--    (((anArray[(10)])))
-- into
--    anArray[10]

flatten :: Expr -> Expr
-- base case: do nothing to literals
flatten (Literal i) = Literal i

-- this is the important case: we shed the Paren constructor and just
-- apply `flatten` to its contents
flatten (Paren e) = flatten e

-- all the other cases preserve their constructors and just apply
-- the flatten function to their children that are of type `Expr`.
flatten (Index e i)     = Index (flatten e) (flatten i)
flatten (Call e args)   = Call (flatten e) (map flatten args)
flatten (Unary op arg)  = Unary op (flatten arg)
flatten (Binary l op r) = Binary (flatten l) op (flatten r)
```

This code is oppressive, ugly, and unmaintainable. Four out of this function's six lines are dedicated to the simple yet tedious task of ensuring that `flatten` propery recurses into its argument's subexpressions—not only is this boring to write, but any future changes (such as added constructors or fields) to `Expr` will force us to rewrite it. (I'll refer to recursion written in this style as *explicit recursion*, in contrast with the implicit recursion provided by recursion schemes.) In addition, it's extremely easy to make mistakes in this definition—the syntatic noise that the primitive recursion introduces renders it hard to spot a missing recursive invocation of `flatten`, yet even one such omission introduces a critical bug.

We can, however, bring some sanity to this madness by writing a function `apply` that, given a function `f` operating on `Expr`s, applies `f` to each subexpression of a given `Expr`:

```haskell
applyExpr :: (Expr -> Expr) -> Expr -> Expr
-- base case: applyExpr is the identity function on constants
applyExpr f (Literal i) = Literal i

-- recursive cases: apply f to each subexpression
applyExpr f (Paren p) = Paren (f p)
applyExpr f (Index e i) = Index (f e) (f i)
applyExpr f (Call e args) = Call (f e) (map f args)
applyExpr f (Unary op arg) = Unary op (f arg)
applyExpr f (Binary l op r) = Binary (f l) op (f r)
```

By separating out the act of recursing over subexpressions, we can reduce our six-line definition of flatten to two lines. In the body of `flatten`, we need only specify that `Paren` nodes be treated differently than other nodes, relying on the `applyExpr` function to take care of recursion for us:

```haskell
flatten (Paren e) = flatten e
flatten x = applyExpr flatten x
```

This function just got far, far easier to write and maintain. The `apply` function is now responsible for both the base case and the simple recursive case of flattening an expression: all we have to do is define the interesting case, i.e. its handling of `Paren` nodes. *Awesome.*

But let's not get ahead of ourselves. We haven't really prevented any boilerplate or eliminated room for bugs here: `applyExpr` just contains and isolates the boilerplate, and we'd need to write a new `apply` function for each and every new type we define. A sufficiently smart compiler could write them for us. And GHC, being a very smart compiler, can. First, though, we'll have to make this `Expr` data type a little bit more general.

## Parameterized Types

```haskell
data Expr a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Show, Eq)
```

This new definition of `Expr` is identical to our previous one, except here we've added a type variable `a` and replaced all recursive occurrences of the `Expr` type with it. Put another way, we have *parameterized* this type in terms of its subexpressions. As such, we have to change our definition of `applyExpr`: the function we apply to each subexpression can no longer be of type `Expr -> Expr`, but must become `a -> a`: indeed, we can make it `a -> b`, letting the function change the type of an `Expr`'s subexpressions if necessary.

```haskell
apply :: (a -> b) -> Expr a -> Expr b
```

The sharp-eyed among you will notice how similar this function is to the built-in `map` function over lists:

```haskell
-- `map` takes a function (a -> b) and makes it operate on lists containing 'a's
map :: (a -> b) -> [a] -> [b]
```

This is not a coincidence: in fact, the `apply` function is exactly analogous to `map` for lists—you can think about both functions as mapping or promoting a function `f` so as to operate on a larger datatype, whether that's an `Expr` type or a list (`[]`) type. This pattern of mapping is so common that its generalized version is a central Haskell concept: the typeclass `Functor` represents all the types that provide a `map`-like function, called `fmap`[^fmap]:

```haskell
class Functor f where
  fmap :: Functor f => (a -> b) -> f a -> f b
```

Countless datatypes—lists, trees, optional (`Maybe`) values, IO actions, even functions themselves—implement the `Functor` typeclass. Indeed, it's so common, and implementing `fmap` is usually so straightforward, that GHC provides a built-in mechanism to write the definition of `fmap` for you: we can just add `Functor` to the list of classes our `Expr` declaration derives, along with `Show` and `Eq`:

```haskell
{-# LANGUAGE DeriveFunctor #-}

data Expr a
  = Index a a
  | Call [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Show, Eq, Functor) -- fmap for free
```

In addition, you can derive instances of the `Foldable` and `Traversable` typeclasses, which provide dozens of of useful functions to access and iterate through an `Expr`'s subexpressions—in essence, `Expr` now comes with batteries included. Parameterizing `Expr` and deriving `Functor`, `Foldable`, and `Traversable` provides us with an embarrassment of helper functions—but this parameterized version of `Expr` isn't quite the same as our previous defintion!

Our first formulation of `Expr`, since its recursive subfields were of type `Expr`, could represent arbitrarily-nested `Expr`s, but this new one can't—it seems like we always have to insert `Lit`—to establish the maximum possible depth of a tree of `Expr`s:

* `Expr Lit` represents an expression with no subexpressions
* `Expr (Expr Lit)` represents expressions with at most one more layer of subexpressions.
* `Expr (Expr (Expr Lit))` represents two-level expressions, and so on, and so forth.

In order for the parameterized definition of `Expr` to be equal to our original formulation, we have to assume that there exists a type such that, when substituted for `a` in the definition of `Expr a`, yields an expression with arbitrarily-nested `Expr` subexpressions.

```haskell
type NestedExpr = Expr (Expr (Expr (Expr …)))
```

But in order for our assumption about the type variable `a` to hold true, we need some sort of trick that allows us to represent, in a finite manner, a representation of the type of arbitrarily-nested `Expr`s.

## Fixed Points

Consider the Y-combinator. Given a function f that takes one argument, `y(f)` represents the result of repeatedly applying `f` to itself:

```haskell
y(f) = f(f(f(f(f ...))))
```

The sharp-eyed will have noticed that the expansion of `y(f)` is very similar to our `NestedExpr` type above. If we have a Y-combinator embedded entirely *in the type system*, we can describe the repeated application of `Expr` to itself, in a manner identical to how the value-level Y-combinator operators on functions, and in turn we can describe an `Expr a` where `a` represents arbitrarily-nested `Expr`s.

```haskell
type Y t = t (t (t (t (t ...))))
```

This general concept[^fixed] is known as 'fixed-point': we say that `y(f)` is the fixed point (or fixpoint) of the `f` function, and that `Y Expr` is the *fixed point of the `Expr` functor*. And here's the kicker—we can build a Y-combinator that works *in the type system* too, and that's is how we will express the self-similar nature of an `Expr`'s subexpressions.

We need a data type `Y` that, when given another type `f`, wraps an `f` whose children are of type `(Y f)`. Let's call it `Term`, and let's call its constructor `In`, representing the fact that we are stuffing one level of recursion into a fixed form. In addition, we'll define an `out` function that unwraps a `Term`.

```haskell
data Term f = In (f (Term f))

out :: Term f -> f (Term f)
out (In t) = t
```

It's illuminating to substitute `Expr` in for the type variable in the above definition:

```haskell
Term Expr = In (Expr (Term Expr))

out :: Term Expr -> Expr (Term Expr)
```

From this definition, we can see that, given a `Term Expr`, we can use the `out` function to convert it to an `Expr` the subexpressions of which are, in turn `Term Expr`s. That means that we can unwrap a `Term Expr` into an *arbitrarily-nested* `Expr` through successive applications of `out`: our `Term Expr` can expand into an `Expr (Term Expr)`, which can expand into an `Expr (Expr (Term Expr))`, and so on and so forth. This style of defining recursive types using fixed-points of functors is an example of *codata*. A full discussion of the theory behind codata (and the many different forms that codata can take) is, unfortunately, beyond the scope of this article; I recommend [this][codata] excellent introduction.

[codata]: http://www.tac-tics.net/data-vs-codata


## Generic Traversals

At this point, we're well grounded in defining our data types with fixed-points of functors. Let's do something awesome with them.

Consider the notion of the bottom-up traversal: specifically, the following algorithm for traversing the fixed-point of a functor:

1. Unpack the term so as to access its children.
2. Recursively traverse each child of the unpacked term with ƒ.
3. Repack the term.
4. Apply ƒ to it.

We have the tools to express each step of this procedure—let's call it `bottomUp.`

```haskell
bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
```

Given a function `fn` from `Term`s to `Term`s, we'll unpack the `Term` with the `out` function, recursively traverse each child of the unpacked term with `fmap (bottomUp fn)`, repack the term with the `In` constructor, and then simply apply `fn` to the result. The `fmap bottomUp` call does all the heavy lifting in this function: it captures the act of recursing into each child (if any) of a given functor.

Rather than naming both the `fn` function parameter and the `Term` parameter, I'm going to define `bottomUp` using combinators to join these four invocations—`out`, `fmap bottomUp`, `In`, and `fn`. Namely, I'm going to use the `>>>` operator, defined in [`Control.Arrow`][arrow], for left-to-right function composition,`f >>> g x` is equal to `g(f(x))`. Though this style is a bit unconventional—the right-to-left function composition operator, `.`, is more common—I've chosen to do this because it's a useful visual indicator of the order in which functions are invoked. (This order will become important later.)

[arrow]: http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Arrow.html

So now let's write this function, gluing each element together left-to-right with the `>>>` operator:

```haskell
bottomUp fn =
  out                    -- 1) unpack
  >>> fmap (bottomUp fn) -- 2) recurse
  >>> In                 -- 3) repack
  >>> fn                 -- 4) apply
```

And there it is, our first recursion scheme. In writing `bottomUp` we have developed a type-safe *and* type-generic combinator for recursively transforming *any* Functor: whether it's our `Expr` type from earlier, a list, a rose tree, or anything else. This is, frankly, kind of amazing. As such, let's rewrite our original `flatten` function so that it operates on `Term`s that wrap arbitrarily-nested `Expr`s:

```haskell
flattenTerm :: Term Expr -> Term Expr
flattenTerm (In (Paren e)) = e  -- remove all Parens
flattenTerm other = other       -- do nothing otherwise

flatten :: Term Expr -> Term Expr
flatten = bottomUp flattenTerm
```

Though our previous definition of `flatten` that used `apply` to represent its recursion was concise, this is even more elegant: our `bottomUp` recusion scheme lets us factor out the recursive parts of this definition entirely. We can focus on the relevant behavior of the flattening function—namely, that it removes all `Paren` nodes—and define it in two simple clauses. In addition, recursively invoking this function with `bottomUp flattenTerm` is clearer than our prior definitions in that we have made the bottom-up nature of this traversal explicit. This is really a remarkable departure from our previous definition of `flatten`—it's hard to imagine how it could be made shorter.

But let's not rest on our laurels. Let's consider the steps involved with writing a top-down traversal of a `Term`, the obvious analogue to our bottom-up traversal:

To traverse a Term top-down with a function ƒ:
1. Apply ƒ to the term.
2. Unpack the term so as to access its children.
3. Recursively traverse each child of the term with ƒ.
4. Repack the term.

These instructions are elegantly symmetrical with the ones for our bottom-up traversal—if you read the instructions in reverse and replace occurrences of "unpack" and "repack", they are identical. And here's the kicker: our code can capture this. We can express this notion of  "reading in reverse" by replacing occurrences of the left-to-right operator `>>>` with `<<<`, the right-to-left operator[^composition], and we swap "unpack" and "repack" with `out` and `In`.

```haskell
topDown, bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f

topDown f  = In <<< fmap (topDown f) <<< out <<< f

bottomUp f = out >>> fmap (bottomUp f) >>> In >>> f
```

The fact that we can express the duality between top-down and bottom-up traversals merely by "reversing the arrows" that determine our code's flow, all the while retaining generality and type safety, is nothing short of amazing. That these definitions emerged naturally out of fixed-points and functors, two concepts central to Haskell and to functional programming in general, is doubly amazing.

### We're Done, Finally

Top-down and bottom-up traversals are the simplest of recursion schemes—we've barely touched the surface of what *Bananas, Lenses, Envelopes, and Barbed Wire* has to offer us. In the next installment of this series I'll explore the myriad varieties of recursion schemes—apomorphisms, paramorphisms, and histomorphisms, just to name a few—and how generalizing each recursion scheme allows us to derive new, more-general schemes.

I'd like to thank everyone who read a draft of this entry, especially Nate Soares and Manuel Chakravarty. I'd also like to thank Colin Barrett, who helped me puzzle all this out over late-night Skype sessions. If you have any comments or questions, please drop me a line on [Twitter](https://twitter.com/importantshock).

[^notation]: Rather than tying *Bananas, Lenses, Envelopes and Barbed Wire* to any particular programming language, Meijer et. al used notation derived from [Bird-Meertens formalism][bird-meertens], a calculus of program construction based on recursion schemes. (Meijer's Ph.D. thesis discussed compiler specifications using the Bird-Meertens formalism). This calculus was also known as "Squiggol", after its "squiggly" notation. Though this notation is well-specified, its syntactic constructions, featuring elements such as “banana brackets” and “concave lenses”, is somewhat abstruse.

[bird-meertens]: http://en.wikipedia.org/wiki/Bird–Meertens_Formalism

[^fixed]: A complete discussion of the beauty and notability of fixed-point combinators is beyond the scope of this article: for such explorations, please refer to Raymond Smullyan's wonderful [_To Mock a Mockingbird_][mockingbird] or Reginald Braithwaite's [Kestrels, Quirky Birds, and Hopeless Egocentricity][combinators].

[mockingbird]: http://www.amazon.com/To-Mock-Mockingbird-Other-Puzzles/dp/0192801422
[combinators]: https://leanpub.com/combinators

[^fmap]: You may be curious as to why Haskell provides both `map` and `fmap` functions in its Prelude, considering that `map` is just a version of `fmap` that can only operate on lists. This has indeed been a bone of contention within the Haskell community. As Brent Yorgey, author of the essential [Typeclassopedia][typeclassopedia], put it: "the usual argument is that someone just learning Haskell, when using `map` incorrectly, would much rather see an error about lists than about `Functor`s."

[typeclassopedia]: http://www.haskell.org/haskellwiki/Typeclassopedia#Instances

[^composition]: This function is provided by the Prelude with the `.` operator.

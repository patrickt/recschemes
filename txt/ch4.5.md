In an effort to publish more than one blog post a year, I've decided to write about smaller topics. Today I'm going to talk about the notion of a 'base functor', and how the popular [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes) library uses base functors to make recursion schemes more elegant and ergonomic in practice.

### Repeating Ourselves

Throughout this series of posts, we've seen the pattern of parameterizing our data types in terms of a recursive type variable. In the first installment, we went from this:

```haskell
data Expr
  = Index Expr Expr
  | Call Expr [Expr]
  | Unary String Expr
  | Binary Expr String Expr
  | Paren Expr
  | Literal Int
  deriving (Show, Eq)
```

to this---the same data type, but of kind[^1] `* -> *`, with all recursive occurrences of the `Expr` type replaced with the type variable `a`:

``` {.sourceCode .literate .haskell}
data ExprF a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Int
  deriving (Show, Eq, Functor)
```

We then used the `Term` trick, a Y-combinator for the type system, to parameterize `ExprF` in terms of itself, yielding a data type equivalent to the original `Expr` but that can be mapped over with `fmap` and folded with `cata`:

``` {.sourceCode .literate .haskell}
newtype Term f = In { out :: f (Term f) }

type Expr = ExprF (Term ExprF)
```

Similarly, in part 4 we represented the natural numbers with a data type of kind `* -> *`:

``` {.sourceCode .literate .haskell}
data Nat a
    = Zero
    | Next a
    deriving Functor
```

And we represented the `Plant` type we grow with another such data type:

``` {.sourceCode .literate .haskell}
data Plant a
  = Root a
  | Stalk a
  | Fork a a a
  | Bloom
```

This is a good and fluent way to build data types. However, it has its disadvantages.

Consider the humble linked list, containing values of type `a`: `[a]`.

``` haskell
infixr 5 :
data [] a = a : [a]
          | []
```


Certainly `cata`, the fundamental right-fold operation, should be able to support folding over this structure: after all, it's of kind `* -> *`. But if we apply *Term* to this data type, we run into trouble quite quickly: we *lose the ability to store elements in a list*. The type variable `a` can only hold nested `Term a` values: there is no place we can actually store list elements. This is, as you can tell, not particularly useful.

Now, of course, we can do our standard song-and-dance routine, converting `[a]` into a type where its recursive element is replaced by a new type variable to represent recursive occurrences (the `[a]` in the `(:)` constructor:)

``` {.sourceCode .literate .haskell}
data ListF a b
  = Cons a b
  | Nil
    deriving (Show, Eq, Functor)
```

This looks a little different from our prior examples---it's of kind `* -> * -> *`, not `* -> *`---but the principle is the same. We add a new type variable, here called `b`, to represent recursive occurrences of the `List` type. And now we can fold over a list:

``` literate-haskell
listSum :: Num a => Algebra (ListF a) a
listSum (Cons a b) = a + b
listSum Nil = 0
```



But this is clumsy. There's something fundamentally wrong if we have to write a replacement for the venerable `[]` type. Recursion schemes are meant to be elegant---manually creating alternate implementations for every recursive data type doesn't strike me as particularly elegant. But I am, thankfully, not alone in feeling this way---the literature contains many ways to work around this. I'll focus on the solution provided by the [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes) package.

### Base Functors to the Rescue {#base-functors}

The first thing you see when you open up the `recursion-schemes` documentation is the following type family declaration:

This understandably intimidates a number of people, especially given its lack of documentation. But this is less forbidding then it appears, and the added mental overhead is worth it---the presence of the `Base` type family is one of the things that sets `recursion-schemes` apart from its competition in terms of ease-of-use.

The purpose of the `Base` type family is to tie together a standard Haskell data type---such as our original `Expr` formulation, or the humble `[]` list---with its parameterized, \"base\" representation. Its definition is very concise:

``` {.sourceCode .literate .haskell}
type family Base t :: * -> *
```

While a full-fledged tutorial on type families is beyond the scope of this post (the best such reference is the [GHC wiki](https://wiki.haskell.org/GHC/Type_families)), we can think of type families as a way to write functions on the type-level. If we were to declare a type family, and an instance of this family (analogous to an instance of a typeclass):

```haskell
type family Something t

type instance Something Foo = Bar
```

then anywhere we encounter the invocation `Something Foo` the GHC type system will resolve that to `Bar`. While this may seem unnecessary---if you mean `Bar`, why not just write `Bar`?---it provides us a rich facility with which to *associate* a type with another.

Look at the definition of `Base` again. The kind signature there indicates that, whenever you pass in a concrete type as the variable `t`, you will yield a data type parameterized with one additional variable. This corresponds to our experience with `ExprF` and `List`: `Expr` went from kind `*` to `* -> *`, and `[a]` went from kind `* -> *` to `* -> * -> *`.

The `Base` type family doesn't tell us much on our own. The most illustrative path open to us is to look at an instance declared with `Base`.

``` {.sourceCode .literate .haskell}
type instance Base [a] = ListF a
```

This declares a type family instance. Anywhere we mean to read `ListF a`, we can write `Base [a]`. This provides important organizational meaning: there is only one sensible parameterized implementation for any recursive type, and thus (thanks to Haskell's support for type families) there is only one implementation of `Base a` for any given type `a`.

This isn't particularly exciting on its own. The real fireworks start here, with the definition (simplified here for pedagogical purposes) of the `Recursive` typeclass.

``` {.sourceCode .literate .haskell}
class (Functor (Base t)) => Recursive t where
  project :: t -> Base t t
  cata    :: (Base t a -> a) -> t -> a
```

The *Recursive* typeclass is similar to the `Foldable` typeclass[^2]. The analogy holds: if we define a `Recursive` instance for our data type `t`, whether that's `Expr` or `[a]` or anything else, we can fold over this type. However, instead of providing a simple fold, we provide two functions: a `project` function that takes a type `t` and returns the same type but transformed into its `Base` form, and a `cata` function that, given an algebra from `Base t a` to `a` (to wit: `Base t a -> a`), and an initial `t` value over which to fold, yields us a folded `a` value.

This is, at first glance, more unwieldy than our formulation of `cata`, which only involved a `Functor` constraint, and nothing related to `Base` functors or a new `Recursive` type:

    cata :: (Functor f) => Algebra f a -> Term f -> a
    cata f = out >>> fmap (cata f) >>> f

But, critically, this formulation of `cata` forces us to work with `Term List` rather than the simple `[a]`. However, `Recursive` allows us to work with ordinary data types: rather than provide a `Term t` to `cata`, we give an ordinary `t`. `Recursive` instances *use the `project` function to transform the provided type into a parameterized one*, then pass that transformed data type to the `cata` function. As such, we can now use `cata` to fold over an ordinary list, without having to wrap it in `Term`s and `Cons` values.

``` {.sourceCode .literate .haskell}
-- yields 10
sumList :: Num a => [a] -> a
sumList = cata go where
  go Nil = 0
  go (Cons a acc) = a + acc
```

`Recursive` has further tricks up its sleeve. Thanks to a `MINIMAL` pragma, the `Recursive` class only needs an implementation of `project` to implement `Recursive` fully---we still get a universal `cata` function for free. The implementation looks like this, if you're interested:

```haskell
class Functor (Base t) => Recursive t where
  project :: t -> Base t t

  cata :: (Base t a -> a) -- ^ a (Base t)-algebra
       -> t               -- ^ fixed point
       -> a               -- ^ result
  cata f = c where c = f . fmap c . project
```


Note that the `cata` function is given a default definition. If you had some magic data type that admitted a faster `cata` than the default implementation, you could override it---however, I struggle to think of a type which would admit a custom `cata`.

Contained inside the `Recursive` class are other useful folds---`para`, the paramorphism, which we discussed in part 3, and ones we haven't covered yet---the generalized paramorphism `gpara` and Fokkinga's prepromorphism `prepro`. (We will discuss these in a future installment).

Note that the Base type is constrained in Recursive instances: `t` must have a `Base` instance, and the `Base` instance for `t` must be a `Functor`. Since `cata` is defined in terms of a recursive invocation with `fmap`, we need a useful `fmap` function over any `Base` instance to have a `cata` that typechecks.

Thanks to the `Recursive` typeclass, we can deal in simple data types---`[a]` rather than `ListF`, `Expr` rather than `ExprF`---while retaining the expressive folding power granted by paramterized data types. This is cool as hell. This technique is used in other libraries, such as José Pedro Magalhães's [`regular`](https://hackage.haskell.org/package/regular/docs/Generics-Regular-Base.html#t:PF).

The implementation of `Recursive` for `[a]` follows. We convert the empty list `[]` into `Nil`, and replace `:` with the `Cons` constructor.

``` {.sourceCode .literate .haskell}
instance Recursive [a] where
  project (x:xs) = Cons x xs
  project [] = Nil
```

Another valuable instance is one for `Natural`---as we discussed in the previous installment, we can fold over the natural numbers, bottoming out when we hit zero. We built our own `Nat` data type, observing that it was equivalent in definition to `Maybe`---`recursion-schemes` just uses `Maybe` for its `Recursive` and `Base` instance for `Natural`.

``` {.sourceCode .literate .haskell}
type instance Base Natural = Maybe

instance Recursive Natural where
  project 0 = Nothing
  project n = Just (n - 1)
```

### Yet More Concision

As we've mentioned before, given a data type `t`, the steps for constructing its `Base` instance are straightforward: add a new type variable to the definition, and for each data constructor, create a new constructor with all recursive occurrences of `t` replaced by the new type variable.

Thanks to the magic of Template Haskell, `recursion-schemes` can generate this code for us:

```haskell
import Data.Functor.Foldable.TH

data Expr
  = Index Expr Expr
  | Call Expr [Expr]
  | Unary String Expr
  | Binary Expr String Expr
  | Paren Expr
  | Literal Lit
  deriving (Show, Eq)

makeBaseFunctor ''Expr
```

The `makeBaseFunctor` call generates code equivalent to the following:

```haskell
data ExprF a
  = IndexF a a
  | CallF a [a]
  | UnaryF String a
  | BinaryF a String a
  | ParenF a
  | LiteralF Lit
  deriving (Show, Eq, Functor)

type instance Base Expr = ExprF

instance Recursive Expr where
  project (Index a b) = IndexF a b
  project (Call a b)  = CallF a b
  -- and so on and so forth
```

This is clearly the result of applying the aforementioned algorithm to our `Expr` type. To avoid name collisions, constructor names are suffixed with an 'F'. (Infix operators are suffixed with a '\$').

The inclusion of these Template Haskell splices means that you, the programmer, can pick up the `recursion-schemes` library with a minimum of fuss and boilerplate. This is, in my experience writing Haskell in production, truly invaluable when dealing with nested data types: you can fold beautifully without setting up dozens of lines of boilerplate.

### Reversing the Arrows, Again {#reversing-the-arrows}

In part two of this series, we generated an unfold by 'reversing the arrows' in `cata`. As you might be able to infer from the title of this section, we can do the same for `Recursive`, which yields us a `Corecursive` typeclass for unfolds:

``` {.sourceCode .literate .haskell}
class Functor (Base t) => Corecursive t where
  embed :: Base t t -> t
  ana :: (a -> Base t a) -> a -> t
```

We've already reversed the arrows and generated `ana` from `cata`. The only thing left to do is reverse the arrows in `project`, yielding `embed`---rather than going from a `t` to a `Base` functor, as `project` does, we go from a `Base` functor to a `t`.

As with `cata`, `ana` is defined in terms of `fmap` and `embed`:

```haskell
class Functor (Base t) => Corecursive t where
  embed :: Base t t -> t
  ana
    :: (a -> Base t a) -- ^ a (Base t)-coalgebra
    -> a               -- ^ seed
    -> t               -- ^ resulting fixed point
  ana g = a where a = embed . fmap a . g
```

Instances for `embed` are similarly straightforward:

```haskell
instance Corecursive [a] where
  embed (Cons x xs) = x:xs
  embed Nil = []
```
In practice, you won't need to write your own `Corecursive` instances, as `makeBaseFunctor` creates both `Recursive` and `Corecursive` instances.

### One More Aside

Particularly alert readers will notice that the definition for `cata` provided by Kmett in `recursion-schemes` is slightly different from ours. Our definition used partial application of `cata` in its definition---`cata f`, being partially applied to `f`, can be passed to `fmap`:

cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

By contrast, Kmett's `cata` uses a where-clause to capture `cata f` with a specific name, `c`.


cata :: (Base t a -> a) -> t -> a
cata f = c where c = f . fmap c . project

Both formulations of `cata` are defined point-free, but Kmett's struck me as somewhat unusual--- the name `c` appears unnecessary, given that you can just pass `cata f` to `fmap`. It took several years before I inferred the reason behind this---GHC generates more efficient code if you avoid partial applications. Partially-applied functions must carry their arguments along with them, forcing their evaluation process to dredge up the applied arguments and call them when invoking the function. whereas bare functions are much simpler to invoke. (For more of the gory details, you can consult the GHC wiki page on the representation of [heap objects](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects)).

Bearing this implementation detail in mind, this formulation of `cata` is quite elegant: by naming `cata f`, we can reference it not as a partially applied function, but as a regular function. Passing that function into `fmap` generates more efficient code---usually this kind of microoptimization doesn't matter much, but given that `cata` is invoked on every iteration of a fold, the savings add up.

### That's All

I owe thanks to Edward Kmett, whose `recursion-schemes` library is magnificent and inspiring, and to Austin Seipp, who checked my statements about GHC code generation for accuracy.

I'm hoping to be done with the next proper installment of this series within the next couple of weeks. (This one covers hylomorphisms and chronomorphisms.) Until then, thank you for reading.

[^1]: If you're not familiar with the notion of a 'kind', you can think (loosely) about the kind of a data type `t` as a measure of how many arguments it takes. Our `Expr` type takes no arguments, so its kind is \"star\": `*`. A data type that takes one argument has kind 'star to star', `* -> *`. A data type that takes three arguments, like `Either`, has kind `* -> * -> *`. The high-level description of a kind is 'the type of a type', but you can think about them as merely providing information as to the parameters taken, if any, by a data type. (The `:k`) directive in GHCi provides information on the kind of any type or typeclass you provide.

[^2]: As you can see by the name of its containing module, `Data.Functor.Foldable`. This class was originally called `Foldable`, but the presence of `Foldable` in the standard library made the duplication unsupportable, and it was changed to `Recursive`

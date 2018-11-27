*Previous installments: [1](http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/), [2](http://blog.sumtypeofway.com/recursion-schemes-part-2/), [3](http://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/), [4](http://blog.sumtypeofway.com/recursion-schemes-part-iv-time-is-of-the-essence/), [4½](http://blog.sumtypeofway.com/recursion-schemes-part-41-2-better-living-through-base-functors/).*

Thus far, we've explored a myriad array of recursion schemes. Catamorphisms and anamorphisms fold and unfold data structures, paramorphisms and apomorphisms fold with additional information, and histomorphisms and futumorphisms allow us to fold using historical values and unfold with user-defined control flow.

Given each fold---`cata`, `para`, `histo`---we derived its corresponding unfold by 'reversing the arrows' of the fold---put another way, we computed the categorical dual of each fold operation. Given the fact that we can derive an unfold from a fold (and vice versa), and given the powerful tool in our toolbox that is function composition, an important question we can ask is "what happens when we compose an unfold with a fold?" In this entry, we'll explore the structures generated from such compositions. (This post is literate Haskell; you can find the code [here](https://github.com/patrickt/recschemes/blob/master/src/Part5.lhs).)

Meijer et. al answered the above question in *[Bananas, Lenses, Envelopes, and Barbed Wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf)*. They called this concept---unfolding a data structure from a seed value, then computing a final result by folding over the data structure thus produced ---a hylomorphism[^1]. The hylomorphism is sometimes referred to as a 'refold', which is a slightly more approachable but not particularly illustrative name---I prefer to think of a hylomorphism as a 'producer-consumer function', where the unfold produces values for the fold to consume.

If you grasp the concept of a catamorphism (a fold) and an anamorphism (an unfold), a hylomorphism is easy: it's just the latter followed by the former. The definition follows:

```haskell
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = ana coalg >>> cata alg
```

Pretty straightforward, right? Unfold with `ana` and the provided coalgebra, then fold with `cata` and the provided algebra.

The 'hylo' in 'hylomorphism' comes from the Greek *hyle*, ὕλη, meaning 'matter'. The ancient Greeks used 'matter' to mean the substance out of which an object is formed ('morpho'); as such, we can read 'hylomorphism' as a function that forms a result object out of some intermediate, constituent matter.

Something interesting to note is that `Term`, the fixed point of a `Functor`, appears nowhere in the signature of `hylo`. Though `ana` produces and `cata` consumes a `Term f`, this is elided in the type signature, a hidden detail of the implemementation: all that is necessary is a `Functor` instance to parameterize the algebra and coalgebra. (Of course, your input `a` or your output `b` could be a `Term` over some `Functor`. But it doesn't have to be.) Similarly, [Kmett's formulation](https://hackage.haskell.org/package/recursion-schemes-5.0.2/docs/Data-Functor-Foldable.html#v:hylo) of hylo makes no `Base` functor visible.

### Highs, Lows, and `hylo`

The hylomorphism is more than an elegant result---it generalizes many computations that we as programmers encounter in our day-to-day work. The canonical example is the factorial function, but an abstraction that encapsulates building then collapsing a data structure is far more useful than yet another cute way to compute `fac(n)`. Though we often don't notice the underlying generality, we're using hylomorphisms every time we:

-   aggregate and compute properties of data structures, e.g. determining the mean or median or outliers present in a set of numeric data;

-   interpret or compile a final result from some textual representation of a nested structure

-   apply recursive divide-and-conquer techniques, e.g. quicksort, mergesort, or the fast Fourier transform;

-   determine differences between data structures, e.g. edit distance or Levenshtein distance over strings.

Let's put `hylo` to work. We'll build a [RPN calculator](https://en.wikipedia.org/wiki/Reverse_Polish_notation) with `hylo`. Given the string `+ 1 2`, our calculator should compute `1 + 2`, and given `2 1 12 3 / - +` it should calculate `(2 + 1) - (12 / 3)`: every RPN postfix expression has one unambiguous parse, obviating the need for parentheses associated with infix operators. Our coalgebra will unfold a list of operations from a seed (a string), producing a list of numbers and operators, and the algebra will consume the generated list, ultimately yielding a stack of numbers.

As I just mentioned, the input to an RPN calculator consists of two kinds of values: mathematical operations (addition, multiplication, &c.) and integer literals. We'll define a `Token` datatype upon which our calculator will operate:

```haskell
data Token
  = Lit Int
  | Op (Int -> Int -> Int)
```

Note that our `Op` constructor contains a binary function `Int -> Int -> Int`, rather than a string representation of the relevant operation. While this precludes a `Show` instance for `Token`, since functions have no meaningful string representation at runtime, it will simplify the implementation: when we parse an `Op`, we'll store the Haskell function that corresponds to the operator, so that when we perform computations we need only call the stored function with the arguments present on the stack.

We need to be able to read a `Token` out of a string. If we were more principled and honest people, we would use a parsing library like [`megaparsec`](https://hackage.haskell.org/package/megaparsec) or [`trifecta`](https://hackage.haskell.org/package/trifecta), or even a `Maybe` monad to represent parse failures---but in an effort to keep things simple, let's make this function pure using `read`, which fails at runtime if someone decides to get saucy and provide invalid data.

```haskell
parseToken :: String -> Token
parseToken "+" = Op (+)
parseToken "-" = Op (-)
parseToken "*" = Op (*)
parseToken "/" = Op div
parseToken num = Lit $ read num
```

Nothing too difficult here. We pattern-match on a given string; given a mathematical operator, we return an `Op` containing the corresponding Haskell function; otherwise, we use `read` to yield an `Int` and wrap it in a `Lit`.

The easiest way to represent a LIFO stack in Haskell is with a list: we push with a cons operator (`:`) and pop by dropping the first item in the list (`tail`). As such, we'll need a `Term`-compatible (parameterized) list type. Though last time we explored how the `Base` type family allows us to use Haskell's `[]` list type with recursion schemes, we'll roll our own here.

```haskell
data List a b
  = Cons a b
  | Nil
    deriving (Show, Eq, Functor)
```

Now we have a `Token` type to operate on and a `List` type to store tokens. Our next objective is to define a coalgebra that builds a `List` of `Token`s from a `String`. Remember the definition of coalgebras from part II:

``` haskell
type Coalgebra f a = a -> f a
```

The seed value `a` will be a `String`, while the container type `f` will be `List Token`. We'll write the type signature of our coalgebra now:

``` haskell
parseRPN :: Coalgebra (List Token) String
```

Keep in mind that `List Token` here is partially-applied, as `List` has three arguments, being of kind `* -> * -> *`. If we were to expand the `f a`, we would yield the type `List Token String`:

``` haskell
parseRPN :: String -> List Token String
```

This makes sense. In each step of our unfold we return a List value containing a `Token` value and the remaining `String` that we have yet to parse, unless the result is `Nil`, at which point we stop unfolding, yielding the list. Because `Nil` contains no children of type `a` or `b`, an occurrence of `Nil` can assume whatever type we need them to be---here `Token` and `String`.

Now let's implement the body of `rpn`. The simplest case handles the empty string: if there's no more input to parse, we terminate the unfold by returning `Nil`. (`ana` knows to stop unfolding if it encounters `Nil` because the recursive `fmap` calls will cease: `Nil` contains no child nodes into which to recurse.)

``` haskell
parseRPN ""  = Nil
```

The case for a nonempty string is more interesting. Given a string `str`, we take as many characters from it as we can, until we encounter a space. We then pass that chunk into `parseToken`, sticking its result into the `a` field of `Cons`, then drop all spaces in the remainder of the string and stick it into the `b` field of the `Cons`. We'll use Haskell's `span` function to do that, which takes a predicate and returns a tuple containing the items that satisfy the predicate and those that don't.

``` haskell
parseRPN str = Cons token newSeed
  where (x, rest) = span (not . isSpace) str
        token     = parseToken x
        newSeed   = dropWhile isSpace rest
```

Let's look at the function all together:

```haskell
parseRPN :: Coalgebra (List Token) String
parseRPN ""  = Nil
parseRPN str = Cons token newSeed
  where (x, rest) = span (not . isSpace) str
        token     = parseToken x
        newSeed   = dropWhile isSpace rest
```

Not too shabby! Six lines of code, two cases, no compiler warnings. (And this would be even cleaner if we used an actual parser.) If we run `ana parseRPN` with `3 4 +` as a seed value, we yield a result equivalent to the list `Lit 3, Lit 4, Op +, Nil`.

It's time to write our evaluator. Let's consult the definition of an `Algebra`:

``` haskell
type Algebra f a = f a -> a
```

Our container type `f` will be, as in `parseRPN`, a `List Token`. But our result type `a` will differ: rather than operating on strings, we want a stack of integers to which we can append (with `Lit`) and upon which we can operate (with `Op`). Let's make a type alias:

```haskell
type Stack = [Int]
```

And now we can set down a type signature for our evaluator:

``` haskell
evalRPN :: Algebra (List Token) Stack
```

But here we encounter a dilemma! Given a reverse-Polish expression: `2 3 +` or `4 2 5 * + 1 3 2 * + /`, we need to compute the result left-to-right, pushing literals onto the stack and performing the operations we find on the values in the stack. This means our evaluator must work from the left (in the manner of `foldl`) rather than from the right (a la `foldr`). But our old friend `cata` is a right fold---it travels all the way to the `Nil` at the end of the list and then propagates its result from the right. How do we work around this, given that `hylo` provides us no opportunity to reverse the parsed list of tokens (an admittedly kludgy fix)?

The answer is simple---our result type will not be an ordinary `Stack` value. We will instead use a function that takes and returns a `Stack`: `Stack -> Stack`. The ultimate result of this catamorphism will be such a function---we kick off the computation by invoking it with an empty stack. Since the leftmost element was evaluated most recently, *the aggregated function will operate on the leftmost element first*. Further invocations will operate on each subsequent item, left-to-right, until we encounter the `Nil` element and cease computation. And, conveniently, the value we provide to this function at the top-level will be the initial stack that this calculator uses.

If this is difficult to visualize, the following diagram may help:

The fact that we can transform `cata`, a rightward fold, into a leftward fold by switching from computing a value to computing a function on values is utterly staggering to me. By adding the most fundamental concept in functional programming---a function---we yield additional power from `cata`, the lowliest of recursion schemes. This strategy is a well-known idiom in the functional-programming world: this stack is an example of a 'difference structure', similar to the difference list that is used in Haskell's `Show` construct.

Let's rewrite `evalRPN` to use `Stack -> Stack` as its carrier type:

evalRPN :: Algebra (List Token) (Stack -> Stack)

That looks right. Our algebra takes a list of tokens and returns a function that takes and returns a stack. When `hylo` completes, we'll yield such a function; the value that we provide to that function will be used as the initial stack. To check our assumptions, we can expand the definition of evalRPN:

evalRPN :: List Token (Stack -> Stack) -> (Stack -> Stack)

When folding over a list, we need to consider two cases: `Nil` and `Cons`. The `Nil` case falls out quite easily: we simply return the identity function, as there is no data with which we would modify a passed-in stack.

evalRPN Nil stack = stack -- aka `id`

Though we are technically returning a function from `evalRPN`, Haskell allows us to write this function in a more beautiful way: rather than returning an explicit function `λstack -> stack`, we can move that `stack` argument into the parameter list of `evalRPN`[^2].

Now let's handle the case of adding a new value onto the stack. Our `Cons` constructor provides two values: a `Lit` that contains an integer, and our accumulator/carrier type, a function from `Stack` to `Stack`. We'll call that `cont`, since we'll continue evaluation by invoking it. (If, for some reason, we wanted to terminate early, we would return a function that did not invoke the provided continuation.) As such, `evalRPN` will take a stack, push the integer from the `Lit` value onto that stack, and invoke `cont` to continue to the next stage:

evalRPN (Cons (Lit i) cont) stack = cont (i : stack)

The case of applying a function to the stack is similar, except we have to introspect the top two values so as to have some operands to the provided `Op`. As such, we pattern-match on the `Cons` structure over which we are folding in order to pop off its top two values. We then apply those operands to the function inside the `Op`, append that value to the stack, and invoke `cont` to proceed to the next stage.

evalRPN (Cons (Op fn) cont) (a:b:rest) = cont (fn b a : rest)
evalRPN _ stack                        = error ("too few arguments on stack: " <> show stack)

If there are too few values on the stack, we call `error` to bail out[^3]. After applying the function contained in the `Op` value to these two values, we append the result of this function to the remainder of the list, then call the continuation to proceed to the next computational stage.

Let's take a look at the `evalRPN` function, assembled in one place:

```haskell
evalRPN :: Algebra (List Token) (Stack -> Stack)
evalRPN Nil stack                      = stack
evalRPN (Cons (Lit i) cont) stack      = cont (i : stack)
evalRPN (Cons (Op fn) cont) (a:b:rest) = cont (fn b a : rest)
evalRPN _ stack                        = error ("too few arguments on stack: " <> show stack)
```

I find this a lovely definition: it shows clearly that evaluation terminates in the `Nil` case, and continues in the `Cons` cases by virtue of invoking the carried `cont` function.

Now we have a coalgebra (the parser) and an algebra (the evaluator, in continuation-passing style). Let's put it all together---we can start by interrogating GHCi as to the type of passing these to `hylo`.

    λ> :t hylo evalRPN parseRPN
    hylo evalRPN parseRPN :: String -> Stack -> Stack

That makes sense: the `String` parameter is our input, and the `Stack` parameter is the initial value of the RPN machine's stack. So now we can build a top-level `rpn` function that takes a string, invoking the result of `hylo` with the provided string and an empty initial stack:

```haskell
rpn :: String -> Stack
rpn s = hylo evalRPN parseRPN s []
```

We can test this by evaluating it in GHCi:

    λ> rpn "15 7 1 1 + - / 3 * 2 1 1 + + -"
    [5]

Dope.

Though an RPN calculator isn't enormously complicated, I'd argue that our implementation demonstrates the virtue of recursion schemes: by separating *what* we're doing from *how* we're doing it, we draw attention to the meat of the problem---parsing from a string and operating on a stack---without concerning ourselves with the details of aggregating data from an input string or iterating over a parsed sequence of tokens. The machinery of unfolding and folding is all contained within `hylo`: all we have to worry about is the core of our problem. And that's pretty remarkable.

### Further Efficiency

We don't need to invoke cata and ana explicitly to build a hylomorphism. We can build `hylo` just out of the algebra and coalgebra itself.

```haskell
hylo' :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo' alg coalg = coalg >>> fmap (hylo' alg coalg) >>> alg
```

Though this definition is arguably less indicative of the fact that a hylomorphism is the composition of an an anamorphism and catamorphism, it bears a compelling property: it entails half as many calls to `fmap` as does the previous definition.

Our original `hylo` unfolded our `List` to its maximum extent, entailing O(n) calls to `fmap`, where n is the number of tokens passed to `rpn`. Subsequently, that structure is torn down with `cata`, using an additional O(n) calls to fmap. In contrast, this new definition of `hylo` only recurses O(n) rather than O(2n) times: as soon as the unfolding completes and the recursive `fmap` invocations bottom out, each level of the structure is passed directly to `alg` as the stack unwinds. This is a significant optimization, especially for deeply-nested structures!

### Time is Running Out (and In)

Though Meijer et al. introduced the hylomorphism along with the catamorphism and anamorphism, Uustalu and Vene's paper does not mention what happens when you compose a histomorphism and futumorphism. It appears to have taken until roughly 2008 (nine whole years!), when Edward Kmett and the \#haskell IRC channel dubbed it the chronomorphism---chrono (χρόνος) being the prefix related to time.

The definition of the chronomorphism follows from that of the hylomorphism:

```haskell
chrono :: Functor f => CVAlgebra f b -> CVCoalgebra f a -> a -> b
chrono cvalg cvcoalg = futu cvcoalg >>> histo cvalg
```

Pretty straightforward: `futu` unfolds a structure multiple layers at a time (thanks to the power of the free monad), and `histo` tears it down.

Unfortunately, coming up with a useful example of chronomorphisms is a bit more difficult than that of a hylomorphism. The plant-growing example in part IV of this series comes close---we used a futumorphism to generate plant life, but only used a catamorphism, rather than a histomorphism, to render the resulting plant. We could have expressed that catamorphism as a histomorphism, as we showed when we implemented `cata` in terms of `histo`, but bringing in the power of histomorphisms and not using them is pretty pointless. I haven't been able to find a useful or illustrative of `chrono` in action (if you know of one, get in touch!) but I at least have the reassurance that its discoverer himself [can't think of one either.](https://twitter.com/kmett/status/318410115101380608) `chrono` can, however, be used to implement the *dynamorphism*, a scheme specialized towards solving dynamic programming problems, which we will discuss in future installments. (It's possible that Uustalu and Vene neglected to mention the chronomorphism for precisely this reason---it's hard to find a truly compelling use case for it.)

### Taking Shortcuts with Elgot (Co)Algebras

Histomorphisms are useful: building up and then collapsing some intermediate structure is a pattern worth abstracting, as separating 'what' from 'how' always gains us some degree of insight into our code. But in practice, this process of construction and destruction is often interrupted. Perhaps, during the construction of our intermediate structure, we determine that the input data violates our assumptions, requiring us to terminate the construction early; perhaps, during destruction, we enter an optimizable state that allows us to skip future destruction steps.

While we could use failure monads over `hylo` to represent these patterns, a paper by Jiří Adámek, Stefan Milius, and Jiří Velebil, entitled [Elgot Algebras](https://arxiv.org/pdf/cs/0609040.pdf), provides us with a category-theoretical treatment of this pattern, avoiding the hornet's nest that is impurity. Named after Calvin Elgot, an American mathematician who worked for many decades in the intersection between software engineering and pure mathematics, Elgot algebras and coalgebras generalize hylomorphisms, catamorphisms, and apomorphisms in a manner both elegant and useful. The paper itself is *extremely* dense, but Kmett, as per usual, has done the community a great service in translating it to Haskell.

Let's consider the type signature of a hylomorphism. Rather than just repeat our first type signature, let's look at `hylo` after we expand the `Algebra` and `Coalgebra` type synonyms.

``` haskell
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
```

This tells us, given a F-coalgebra over `a` and an F-algebra over `b`, how to get from an `a` to a `b`. But what if we could take a shortcut? If, in our coalgebra (the 'construction' function), we could short-circuit the whole hylomorphism, returning a plain `b` value, we could provide this refold function with an escape hatch---without having to worry about the semantics of `Maybe` or `Either` or `Except` or whatever failure monad we would use with plain `hylo`.

To allow this, our coalgebra, `a -> f a`, has to be able to return one of two values---an `f a`, which continues the unfold, or a `b`, short-circuiting it. Haskell provides us a mechanism to return one of two values, of course: the trusty `Either` type. Changing our coalgebra to return an `Either` type yields us with the type signature for `Elgot`, the Elgot algebra:

```haskell
elgot :: Functor f => Algebra f b -> (a -> Either b (f a)) -> a -> b
```

We'll use an auxiliary functions to define Elgot algebras: ` &&& ` (pronounced 'fanin'). It is an infix form of the `either` helper function: given two functions, one of type `b -> a` and the other of type `c -> a`, it creates a function that takes `Either` a `b` or a `c` and returns an `a`.

``` haskell
(|||) :: (b -> a) -> (c -> a) -> (Either b c -> a)
```

Reading ` ||| ` as 'or' can be a helpful mnemonic: we can see that `f ||| g` returns a function that uses `f` *or* `g`.

Defining `elgot` follows straightforwardly from the above optimized definition of `hylo`. We begin by invoking our coalgebra. If we get a `Right` value out of it, we recurse into it, eventually passing this layer of the computation on to our coalgebra---in other words, it behaves like a normal call to `hylo`. But if we get a `Left` value, we just stop, performing no operation on the value contained therein.

```haskell
elgot :: Functor f => Algebra f b -> (a -> Either b (f a)) -> a -> b
elgot alg coalg = coalg >>> (id ||| (fmap (elgot alg coalg) >>> alg))
```

Let's use an Elgot algebra to bring some sense of safety to our above RPN calculator. Calling `error` on invalid input is a bad look: this is Haskell, and we can do better. Let's start by writing a custom type to represent success and failure.

```haskell
data Result
  = Success Stack
  | ParseError String
  | TooFewArguments Stack
    deriving (Eq, Show)
```

As with the previous incarnation of this function, we'll use continuation-passing style, but instead of a function over `Stack`s, our continuation will handle `Result`s. We're gonna be mentioning functions of type `Result -> Result` a few times here, so we'll make a type alias for it.

```haskell
type Cont = Result -> Result
```

We'll start by rewriting `parseToken`. Rather than returning a plain `Token` and failing at runtime if provided an invalid numeric value, we'll use `readMaybe` to yield a `Maybe Int`, returning an `Either` that contains an early-termination function over `Result`s or an integer wrapped by a `Lit` constructor

```haskell
safeToken :: String -> Either Cont Token
safeToken "+" = Right (Op (+))
safeToken "-" = Right (Op (-))
safeToken "*" = Right (Op (*))
safeToken "/" = Right (Op div)
safeToken str = case readMaybe str of
  Just num -> Right (Lit num)
  Nothing  -> Left  (const (ParseError str))
```

Similarly, we rewrite `parseToken` to invoke `safeToken`. The `do`-notation provided by Haskell beautifies the nonempty-string case, binding a successful parse into the `parsed` result when we encounter a `Right` value and implicitly terminating should `safeToken` return `Left`, the failure case.

```haskell
safeParse :: String -> Either Cont (List Token String)
safeParse ""  = return Nil
safeParse str = do
  let (x, rest) = span (not . isSpace) str
  let newSeed   = dropWhile isSpace rest
  parsed <- safeToken x
  return $ Cons parsed newSeed
```

To tie all this together, we rephrase the definition of `safeEval`. We have to make our pattern-matching slightly more specific---we have to match on `Success` values to get a stack out of them---but we can also remove the call to `error` in this function. When handling an arithmetic operation, if there are too few values on the stack to proceed, we call the continuation with a `TooFewArguments` value

```haskell
safeEval :: Algebra (List Token) Cont
safeEval (Cons (Lit i) cont) (Success stack) = cont (Success (i : stack))
safeEval (Cons (Op fn) cont) (Success s)     = cont (case s of
  (a:b:rest) -> Success (fn b a : rest)
  _          -> TooFewArguments s)
safeEval _ result  = result
```

That's two uses of `error` removed---a victory in the struggle against partial functions and runtime crashes! Furthermore, the error handling becomes tacit in the definition of `safeParse`: no case statements or calls to `throw` are required, as the do-notation over `Either` handles the `Left` case for us.

Invoking these functions is just as simple as it was when we used `hylo`. We replace `hylo` with `elgot`, and pass an empty `Success` value to evaluate the continuation left-to-right over the provided string.

```haskell
safeRPN :: String -> Result
safeRPN s = elgot safeEval safeParse s (Success [])
```

Other examples of using Elgot algebras are few and far between, but the excellent Vanessa McHale has an example of them on [her blog](http://blog.vmchale.com/article/elgot-performance), in which she uses them to calculate the [Collatz sequence](https://esolangs.org/wiki/Collatz_sequence) of a provided integer, yielding performance comparable to an imperative, lower-level Rust implementation.

### Reversing the Arrows, Again

In the defintion of `elgot` above, we used `|||` to handle the Either case: in performing `id` (no operation) on a `Left` value and recursing on a `Right` value, we gained a clarity of definition---but more importantly, we make it easy to reverse the arrows. Every time we reverse the arrows on a fold, we yield the corresponding unfold: but here, reversing the arrows on an Elgot coalgebra, we yield a hylomorphism that can short-circuit during *destruction*, rather than construction.

We know how to reverse most of the operations in the above definition: `alg` becomes `coalg` and vice versa, `>>>` becomes `<<<` and vice versa, and `id` stays the same, being its own dual. The `|||` may be slightly less obvious, but if we remember that the tuple value (`(a, b)`) is dual to `Either a b`, we yield the `&&&` operator, pronounced 'fanout':

``` haskell
(&&&) :: (a -> b) -> (a -> c) -> (a -> (b, c))
```

Whereas `|||` took two functions and used one or either of them to deconstruct an `Either`, `&&&` takes two functions and uses both of them to construct a tuple: given one of type `a -> b` and the other of type `a -> c`, we can apply them both on a given `a` to yield a tuple of type `(b, c)`. Again, reading the triple-ampersand as 'and' can be a useful memonic: `f &&& g` returns a function that uses both `f` 'and' `g`.

Now we know how to reverse every operation in `elgot`. Let's do so:

``` haskell
coelgot alg coalg = alg <<< (id &&& (fmap (coelgot alg coalg) <<< coalg))
```

Feeding this into GHCi and querying its type yields the following lovely signature:

``` haskell
coelgot :: Functor f => ((a, f b) -> b) -> (a -> f a) -> a -> b
```

Our algebra, which previously took an `f b`, now takes a tuple---`(a, f b)`. That `a` is the same `a` used to generate the `f b` we are examining. The 'shortcut' behavior here is slightly more subtle than that present in the definition of `elgot`---it depends on Haskell's call-by-need semantics. If we never observe the second element of the tuple---the `f b`---it will never be evaluated, and as such neither will any of the computations used to construct it!

By replacing `a -> f a` with its natural type synonym, `Coalgebra`, we yield a unified definition of `coelgot`.

```haskell
coelgot :: Functor f => ((a, f b) -> b) -> Coalgebra f a -> a -> b
coelgot alg coalg = alg <<< (id &&& (fmap (coelgot alg coalg) <<< coalg))
```

We can think of Elgot algebras as a hylomorphism built out of an `RCoalgebra` and an `Algebra`. Dually, we can think of Elgot coalgebras as hylomorphisms built out of an `RAlgebra` and a `Coalgebra`. We can also build a more powerful hylomorphism out of both an `RAlgebra` and `RCoalgebra`:

```haskell
rhylo :: Functor f => RAlgebra f b -> RCoalgebra f a -> a -> b
rhylo ralg rcoalg = apo rcoalg >>> para ralg
```

As far as I can tell, this construction (though it is not particularly groundbreaking) hasn't been named in the literature before---if you know its name, drop me a line. I refer to it as an "R-hylomorphism", but I like Rob Rix's term for it, the "hypomorphism", a clever amalgam of its component parts. I leave the deforestation stage, analogous to `hylo`, as an exercise for the reader.

### Acknowledgments

As always, I would like to thank Manuel Chakravarty for his patience and kindness in checking this series for accurately. Colin Barrett, Ross Angle, and Scott Vokes also provided valuable feedback.

I remain very grateful your readership. The next entry will discuss the fusion laws that folds, unfolds, and refolds all possess, and how we can use these laws to make real-world use of these constructs extremely fast.

[^1]: If you Google 'hylomorphism', the results will be almost exclusively concerned with Aristotle's [philosophical theory](https://en.wikipedia.org/wiki/Hylomorphism) of the same name. Though Aristotle's concept is not particularly relevant to the study of recursion schemes, we'll discuss why this name is appropriate for the computation that our hylomorphism performs.

[^2]: Put another way, Haskell makes no syntactic distinction between the types `a -> (b -> c)` and `a -> b -> c`.

[^3]: We could ensure that there are always sufficient values on our stack: if our calculator is initialized with an infinite list for a stack (such as `[0, 0..]`, an infinite sequence of zeroes), we could omit the error case.

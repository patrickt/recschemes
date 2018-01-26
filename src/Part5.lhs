\ignore{
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Part5
  ( Token (..)
  , List (..)
  , parseRPN
  , evalRPN
  , rpn
  , chrono
  ) where

import Part1 (Term (..))
import Part2
import Part4

import Control.Arrow ((>>>))
import Data.Functor.Compose
import Data.Maybe
import Data.Char
import Text.Read (readMaybe)
import Data.Monoid
\end{code}
}

If you grasp the concept of a catamorphism (a fold) and an anamorphism (an unfold), a hylomorphism is easy:
it's just an unfold followed by a fold. The unfold creates a nested structure out of a seed value, and the
fold tears the resulting structure down into a final value. Here's the definition:

\begin{code}
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = ana coalg >>> cata alg
\end{code}

Pretty straightforward, right? Unfold with \texttt{ana} and the provided coalgebra, then fold with \texttt{cata}
and the provided algebra.

The `hylo' in `hylomorphism' comes from the Greek \emph{hyle}, ὕλη, meaning `matter'. The ancient Greeks used
`matter' to mean the substance out of which an object is formed (`morpho`); as such, we can read
`hylomorphism' as a function that forms an object out of some intermediate matter.

Let's use this in practice. We'll build a postfix \href{https://en.wikipedia.org/wiki/Reverse_Polish_notation}{RPN calculator} with \texttt{hylo}: the coalgebra will unfold a list of operations from a seed string, and the
algebra will consume the generated list, ultimately yielding a stack of results.

The stack of an RPN calculator contains two types of values: mathematical operations (addition, multiplication,
&c.) and integer literals. We'll create a \texttt{Token} type that our calculator will store on its stack.

\begin{code}
data Token
  = Lit Int
  | Op (Int -> Int -> Int)
\end{code}

Note that our \texttt{Op} constructor contains a binary function \texttt{Int -> Int -> Int}, rather than some
other data type. While this precludes a \texttt{Show} instance, since functions have no meaningful string
representation, it will simplify the implementation: when we encounter an \texttt{Op}, we will apply the
contained function to further operands in the stack.

We need to be able to read a \texttt{Token} out of a string. If we were more principled and honest people, we
would use a parsing library like \href{\texttt{megaparsec}}{https://hackage.haskell.org/package/megaparsec} or
\href{\texttt{trifecta}}{https://hackage.haskell.org/package/trifecta}, or even a \texttt{Maybe} monad to
represent parse failures—but in an effort to keep things simple, let's make this function pure, calling
\texttt{error} if someone decides to get saucy and provide invalid data.

\begin{code}
parseToken :: String -> Token
parseToken "+" = Op (+)
parseToken "-" = Op (-)
parseToken "*" = Op (*)
parseToken "/" = Op div
parseToken x   = Lit (fromMaybe (error ("bad token: " ++ x)) (readMaybe x))
\end{code}

Given a mathematical operator, we return an Op containing the corresponding Haskell function; otherwise, we
use \texttt{readMaybe} to yield a \texttt{Lit} value, perishing at runtime if not.

The next thing we'll need is a \texttt{Term}-compatible list type. Though last time we explored how the
\texttt{Base} type family allows us to use Haskell's \texttt{[]} type with recursion schemes, we'll define
our own here.

\begin{code}
data List a b
  = Cons a b
  | Nil
    deriving (Show, Eq, Functor)
\end{code}

Remember the definition of coalgebras from part II:

\begin{verbatim}
type Coalgebra f a = a -> f a
\end{verbatim}

The seed value \texttt{a} will be a \texttt{String}, while the container type \texttt{f} will be
\texttt{List Token}. We'll write the type signature of our coalgebra now:

\begin{verbatim}
parseRPN :: Coalgebra (List Token) String
\end{verbatim}

Keep in mind that \texttt{List Token} here is partially-applied, as \texttt{List} has
three arguments, being of kind \texttt{* -> * -> *}. If we were to expand the \texttt{f a}, we would yield
the type \texttt{List Token String}; this makes sense, as our list holds \texttt{Token} values and the remaining
\texttt{String} that we have yet to parse (unless the result is \texttt{Nil}, at which point we stop unfolding).

Now let's implement the body of our function. The simplest case handles the empty string: if there's no more
input to parse, we terminate the unfold by returning \texttt{Nil}. (\texttt{ana} knows to stop unfolding
if it encounters \texttt{Nil} because the recursive \texttt{fmap} calls will cease: \texttt{Nil} contains
no child nodes into which to recurse.)

\begin{verbatim}
parseRPN ""  = Nil
\end{verbatim}

The case for a nonempty string is more interesting. Given a string \texttt{str}, we take as many characters
from it until we encounter a space. We then pass that chunk into \texttt{parseToken}, sticking its result
into the \texttt{a} field of \texttt{Cons}, then drop all spaces in the remainder of the string and stick
it into the \texttt{b} field of the \texttt{Cons}. We'll use Haskell's \texttt{span} function to do that,
which takes a predicate and returns a tuple containing the items that satisfy the predicate and those
that don't.

\begin{verbatim}
parseRPN str = Cons token newSeed
  where (x, rest) = span (not . isSpace) str
        token     = parseToken x
        newSeed   = dropWhile isSpace rest
\end{verbatim}

Let's look at the function all together:

\begin{code}
parseRPN :: Coalgebra (List Token) String
parseRPN ""  = Nil
parseRPN str = Cons token newSeed
  where (x, rest) = span (not . isSpace) str
        token     = parseToken x
        newSeed   = dropWhile isSpace rest
\end{code}

Not too shabby! Five lines of code, two cases, no compiler warnings. (And this would be even cleaner if we
used an actual parser.) If we run \texttt{ana parseRPN "3 4 +"}, we yield a result equivalent to the list
\texttt{[Lit 3, Lit 4, Op +]}.

It's time to write our evaluator. Let's consult the definition of an \texttt{Algebra}:

\begin{verbatim}
type Algebra f a = f a -> a
\end{verbatim}

Our container type \texttt{f} will be, as in \texttt{parseRPN}, a \texttt{List Token}. This time, however, our
carrier type \texttt{a} will differ: rather than operating on strings, we want a stack of integers to which we can append (with \texttt{Lit}) and upon which we can operate (with \texttt{Op}). Let's make a type alias:

\begin{code}
type Stack = [Int]
\end{code}

And now we can set down a type signature for our evaluator:

\begin{verbatim}
evalRPN :: Algebra (List Token) (Stack -> Stack)
\end{verbatim}

But this is wrong! Here we have an dilemma.

Given a reverse-Polish expression: \texttt{+ 2 3} or \texttt{4 2 5 * + 1 3 2 * + /}, we need to operate
left-to-right: our evaluator must work from the left (in the manner of \texttt{foldl}) rather than from
the right (a la \texttt{foldr}). But our old friend \texttt{cata} is a right fold—it travels all the way
to the \texttt{Nil} at the end of the list and then propagates its result from the right. How do we work
around this?

The answer is simple—our result type will not be an ordinary \texttt{Stack} value. We will instead
use a function that takes and returns a \texttt{Stack}: \texttt{Stack -> Stack}. The ultimate result of
this catamorphism will be a function of type \texttt{Stack -> Stack}—we'll invoke that with an empty stack
as its initial seed value, and the outermost function will execute, invoking the function created by its
subsequent list item, which invokes the function created by \emph{that} item's successor, all until we
reach the \texttt{Nil}.

TODO: diagram

Functional programmers will recognize this as \emph{continuation-passing-style}. And it is!
The fact that we can use CPS to transform a right fold (expressed with \texttt{cata}) into a
left fold is utterly staggering to me.

\begin{verbatim}
evalRPN :: Algebra (List Token) (Stack -> Stack)
\end{verbatim}

That looks right. Our algebra takes a list of tokens and returns
a function that takes and returns a stack. Our pattern matches will either
be over \texttt{Nil}, for which we will return the constant function, or a
\texttt{Cons token cont}, where \texttt{token :: Token} and \emph{cont :: Stack -> Stack}.
At each stage, we'll return a function that performs the appropriate token operation
on its parameter, then invokes the \emph{cont} value to continue to the next stage.

Let's get started. The first case falls out easily: if we hit Nil, then processing is done,
so we return a function that leaves its stack argument untouched.

\begin{verbatim}
evalRPN Nil = λstack -> stack
\end{verbatim}

Now let's handle the case of adding a new value onto the stack. We pattern-match on the
\texttt{Lit} to get out the \emph{Int} value, then return a function that appends that
value to a provided stack, then invokes the continuation to proceed to the next step.

\begin{verbatim}
evalRPN (Cons (Lit i) cont) = λstack -> cont (i : stack)
\end{verbatim}

The case of applying a function to the stack is similar, except our returned function
has to introspect the top two values so as to have some operands to the provided \texttt{Op}.
So we use a \texttt{case} statement that introspects the \texttt{stack} argument to pop off
its top two values, apply those operands to the function inside the \texttt{Op}, append
that value to the stack, and invoke \texttt{cont} to proceed to the next stage.

\begin{verbatim}
evalRPN (Cons (Op fn) cont) = λstack -> case stack of
  (a : b : rest) -> cont (fn b a : rest)
  _              -> error ("too few arguments on stack: " <> show stack)
\end{verbatim}

I wrote this using explicit lambdas to make it clear that each step of the evaluation
returns a function, which invokes a continuations to guide the execution pattern of
the computation as a whole But these lambdas are unnecessary: we can write this more
naturally, making the \texttt{stack} argument part of the function's arguments.

\begin{code}

evalRPN :: Algebra (List Token) (Stack -> Stack)
evalRPN Nil stack                      = stack
evalRPN (Cons (Lit i) cont) stack      = cont (i : stack)
evalRPN (Cons (Op fn) cont) (a:b:rest) = cont (fn b a : rest)
evalRPN _ stack                        = error ("too few arguments on stack: " <> show stack)

\end{code}

Now we have a coalgebra (the parser) and an algebra (the evaluator, in continuation-passing style).
Let's interrogate GHCi as to the type of passing these to \texttt{hylo}.

\begin{verbatim}
λ> :t hylo evalRPN parseRPN
hylo evalRPN parseRPN :: String -> Stack -> Stack
\end{verbatim}

That makes sense: the \texttt{String} parameter is our input, and the \texttt{Stack} parameter is the
initial value of the RPN machine's stack. So now we can build ou

\begin{code}
rpn :: String -> Stack
rpn s = hylo evalRPN parseRPN s []
\end{code}

We can test this by evaluating it in GHCi:

\begin{verbatim}
λ> rpn "15 7 1 1 + - / 3 * 2 1 1 + + -"
[5]
\end{verbatim}

\subsubsection{Deforestation (But Not the Sad Kind)}



One additional note: we don't need to invoke cata and ana explicitly to build a hylomorphism.
We can build \texttt{hylo} just out of the algebra and coalgebra itself.

\begin{code}
hylo' :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo' alg coalg = coalg >>> fmap (hylo alg coalg) >>> alg
\end{code}

This definition is more efficient than the prior definition, as it involves fewer calls to
\texttt{fmap}, but it's slightly less indicative of the fact that a hylomorphism is the composition
of \texttt{cata} and \texttt{ana}.

Though Meijer et al. introduced the hylomorphism along with the catamorphism and anamorphism,
Uustalu and Vene's paper does not mention what happens when you compose a histomorphism and
futumorphism. It appears to have taken until roughly 2008 (nine whole years!), when Edward Kmett
dubbed it the chronomorphism.

\begin{code}
chrono :: Functor f => CVAlgebra f b -> CVCoalgebra f a -> a -> b
chrono cvalg cvcoalg = futu cvcoalg >>> histo cvalg
\end{code}

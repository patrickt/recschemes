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

hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = ana coalg >>> cata alg

data List a b
  = Cons a b
  | Nil
    deriving (Show, Eq, Functor)

data Token
  = Lit Int
  | Op (Int -> Int -> Int)

parseTok :: String -> Token
parseTok "+" = Op (+)
parseTok "-" = Op (-)
parseTok "*" = Op (*)
parseTok "/" = Op div
parseTok x   = Lit (fromMaybe (error ("bad parseToken: " ++ x)) (readMaybe x))

parseRPN :: Coalgebra (List Token) String
parseRPN []  = Nil
parseRPN str = Cons (parseTok x) (dropWhile isSpace rest)
  where (x, rest) = span (not . isSpace) str

type Stack = [Int]

\end{code}

Now, if we run \texttt{ana parseRPN "3 4 +"}, we yield a r

Here we have an apparent dilemma. We need to evaluate this list going from the
left rather than the right—a \texttt{foldl} rather than a \emph{foldr}. But our
old friend \texttt{cata} is a right fold—it travels all the way to the \texttt{Nil}
at the end of the list and then propagates its result from the right. How do we work
around this?

The answer is simple—instead of returning a concrete \texttt{Stack} value at each stage
of the fold, we return a function from \texttt{Stack} to \texttt{Stack}. Calling the accumulated
function parameter (contained inside a \texttt{Cons} structure) will send us to the next stage
of the computation. The ultimate result of this catamorphism will be a function of type
\texttt{Stack -> Stack}—we'll invoke that with an empty stack as its initial seed value, and
the outermost function will execute, invoking the function created by its subsequent list item,
which invokes the function created by \emph{that} item's successor, all until we reach the \texttt{Nil}.

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

evalRPN :: Algebra (List Token) ([Int] -> [Int])
evalRPN Nil stack                      = stack
evalRPN (Cons (Lit i) cont) stack      = cont (i : stack)
evalRPN (Cons (Op fn) cont) (a:b:rest) = cont (fn b a : rest)
evalRPN _ stack                        = error ("too few arguments on stack: " <> show stack)

\end{code}

All that's left to do is to build an evaluator, of type \texttt{String -> Int}, by using
\texttt{hylo} on the \texttt{evalRPN} algebra and the \texttt{parseRPN} coalgebra. We pass
in the provided string to be consumed by \emph{parseRPN}, and an empty list so that the
function returned by \texttt{evalRPN} has a seed value.

\begin{code}
rpn :: String -> Stack
rpn s = hylo evalRPN parseRPN s []
\end{code}

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

\begin{code}

\end{code}

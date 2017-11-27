\ignore{
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
module Part5
  ( change'
  , dyna
  , hylo )
  where

import Prelude hiding (lookup)

import Part1 (Term (..))
import Part2
import Part4

import Control.Arrow
import Data.List hiding (lookup)
\end{code}
}

\begin{code}
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo f g = ana g >>> cata f

chrono :: Functor f => CVAlgebra f b -> CVCoalgebra f a -> a -> b
chrono f g = futu g >>> histo f

cata' :: Functor f => Algebra f a -> Term f -> a
cata' f = hylo f out

ana' :: Functor f => Coalgebra f a -> a -> Term f
ana' = hylo In
\end{code}

CVCoalgebra f (Term f)
Term f -> f (CoAttr f (Term f)

We can't construct an \texttt{f (CoAttr f (Term f)} ourselves—all we can call
on values of type \texttt{f} is \texttt{fmap}, because all we know about \texttt{f}
is that it is a \texttt{Functor}. However, since we've been provided a \texttt{Term f},
we can get an \texttt{f (Term f)} value by calling \texttt{out}. From there, we can
twiddle the internal \texttt{Term f} into a \texttt{CoAttr f (Term f)} by mapping the
\texttt{Automatic} constructor into it.

\begin{code}
histo' :: Functor f => CVAlgebra f a -> Term f -> a
histo' f = chrono f (out >>> fmap Automatic)
\end{code}

\begin{verbatim}
futu' :: Functor f => CVCoalgebra f a -> a -> Term f
futu' g = chrono _ g
\end{verbatim}

\begin{verbatim}
/Users/patrick/src/recschemes/src/Part5.lhs:46:18: error:
    • Found hole: _ :: CVAlgebra f (Term f)
\end{verbatim}

Substituting \texttt{CVAlgebra f (Term f)} into the original definition
of \texttt{CVAlgebra} yields a function of type

\begin{verbatim}
f (Attr f (Term f)) -> Attr f (Term f)
\end{verbatim}

Well, that's trivial. We already have a function of that type: the \texttt{Manual}
constructor.

\begin{code}
dyna :: Functor f => CVAlgebra f b -> (a -> f a) -> a -> b
dyna f g = chrono f (g >>> fmap Automatic)
\end{code}

\begin{code}
change' :: Cent -> Int
change' = dyna go promote where
  promote :: Cent -> Nat Cent
  promote 0 = Zero
  promote n = Next (n - 1)
\end{code}

Take a close look at that. This is \emph{significantly} more efficient than the definition
of \texttt{expand} from the previous installment, which was:

\begin{verbatim}
-- Convert from a natural number to its foldable equivalent, and vice versa.
expand :: Int -> Term Nat
expand 0 = In Zero
expand n = In (Next (expand (n - 1)))
\end{verbatim}

Whereas \texttt{expand} ran in O(n) time, owing to its recursive call, \texttt{promote}
runs in O(1).

\begin{code}
  go :: CVAlgebra Nat Int
  go Zero = 1
  go curr@(Next attr) = let
    given               = compress curr
    validCoins          = filter (<= given) coins
    remaining           = map (given -) validCoins
    (zeroes, toProcess) = partition (== 0) remaining
    results             = sum (map (lookup attr) toProcess)
    in length zeroes + results
\end{code}

\ignore{
\begin{code}
lookup :: Attr Nat a -> Int -> a
lookup cache 0 = attribute cache
lookup cache n = lookup inner (n - 1) where (Next inner) = hole cache
\end{code}
}

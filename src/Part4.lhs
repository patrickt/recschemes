\long\def\ignore{}

Though we've only just begun our dive into \emph{Bananas, Lenses, Envelopes, and Barbed Wire}, the next natural step in understanding recursion schemes brings us outside its purview. We must turn our attention to a paper written seven years later---\href{http://cs.ioc.ee/~tarmo/papers/inf99.pdf}{\emph{Primitive(Co)Recursion and Course-of-Value (Co)Iteration, Categorically}}, by Tarmo Uustalu and Varmo Vene. \emph{Primitive (Co)Recursion} explores and formalizes the definition of apomorphisms (introduced first by Meijer et. al, and which we discussed, briefly, in the \href{http://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/}{previous installment}) and describes two new recursion schemes, the \emph{histomorphism} and the \emph{futumorphism}.

\emph{Primitive (Co)Recursion} is a wonderful and illuminating paper, but it is dense in its concepts for those unfamiliar with category theory, and uses the semi-scrutable bracket syntax introduced by \emph{Bananas}. But there's no need for alarm if category theory isn't your cup of tea: Haskell allows us, once again, to express elegantly the new recursion schemes defined in \emph{Primitive (Co)Recursion}. Guided by Uustalu and Vene's work, we'll derive these two new recursion schemes and explore their ways in which they simplify complicated folds and unfolds. Though these new morphisms are, definition-wise, simple variations on paramorphisms and apomorphisms, in practice they provide surprising power and clarity, as Uustalu and Vene assert:

\begin{quote}
{[}We{]} argue that even these schemes are helpful for a declaratively thinking programmer and program reasoner who loves languages of programming and program reasoning where programs and proofs of properties of programs are easy to write and read.
\end{quote}

That sure sounds like us. Let's get going. This article is literate Haskell; you can find the source code \href{https://github.com/patrickt/recschemes/blob/master/src/Part4.lhs}{here}.

% Now we take care of the Haskell stuff.

\ignore{
\begin{code}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE OverloadedStrings    #-}

module Part4
  ( change )
  where
import           Part1                  (Term (..))
import           Part2
import           Part3                  (RAlgebra, RCoalgebra)
import           Prelude                hiding (lookup)

import           Control.Arrow          hiding (left, right)
import           Data.List              hiding (lookup)
import qualified System.Random          as Random
import           Text.PrettyPrint.Boxes
\end{code}
}

\subsubsection{A Brief Recap}\label{a-brief-recap}

In our first entry, we defined \texttt{Term}, the fixed-point of a
Haskell \texttt{Functor}, with an \texttt{In} constructor that wraps one
level of a structure and an \texttt{out} destructor to perform the
corresponding unwrap\footnote{
  Bob Harper, in \emph{Practical Foundations
  for Programming Languages}, refers to \texttt{In} and \texttt{out} as
  ``rolling'' and ``unrolling'' operations. This is a useful visual
  metaphor: the progression
  \texttt{f (f (Term f)) -> f (Term f) -> Term f}
  indeed looks like a flat surface being rolled up, and its opposite
  \texttt{Term f -> f (Term f) -> f (f (Term f))}
  looks like the process of unrolling.}.

\begin{verbatim}
newtype Term f = In { out :: f (Term f) }
\end{verbatim}

Given an algebra --- a folding function that collapses a Functor
\texttt{f} containing \texttt{a}'s into a single \texttt{a}---

\begin{verbatim}
type Algebra f a = f a -> a
\end{verbatim}

we use the catamorphism \texttt{cata} to apply a leaf-to-root\footnote{
  Rob Rix \href{https://twitter.com/rob_rix/status/793430628637274112}{points out}
  that, though catamorphisms are often described as ``bottom-up'',
  this term is ambiguous: catamorphisms' recursion occurs top-down, but
  the folded value is constructed bottom-up. I had never noticed this
  ambiguity before. (The words of Carroll come to mind: ``\,`When I use
  a word,' Humpty Dumpty said, in rather a scornful tone, `it means just
  what I choose it to mean --- neither more nor less.'\,'')}
fold over any recursively-defined data structure. \texttt{cata} travels to the
most deeply-nested point in the data structure by \texttt{fmap}ing
itself, recursively, into the next level of the stucture. When
\texttt{fmap\ cata\ x} returns an unchanged \texttt{x}, we cease
recursing (because we have hit the most-deeply-nested point); we can
then begin constructing the return value by passing each node to the
algebra, leaf-to-root, until all the recursive invocations have
finished.

\begin{verbatim}
cata :: (Functor f) => Algebra f a -> a -> Term f
cata f = out >>> fmap (cata f) >>> f
\end{verbatim}

But the catamorphism has its limits: as it is applied to each level of
the structure, it can only examine the current carrier value from which
it is building. Given the F-algebra \texttt{f\ a\ -\textgreater{}\ a},
each of the structure's children---the \texttt{a} values contained in
the \texttt{f} container---has already been transformed, thus losing
information about the original structure. To remedy this, we introduced
\texttt{para}, the paramorphism, and an R-algebra to carry the original
structure with the accumulator:

\begin{verbatim}
type RAlgebra f a = f (Term f, a) -> a

para :: Functor f => RAlgebra f a -> Term f -> a
para f = out >>> fmap (id &&& para f) >>> f
\end{verbatim}

\subsubsection{Running a Course with
Histomorphisms}\label{running-a-course-with-histomorphisms}

Paramorphisms allow us, at each stage of the fold, to view the original
structure of the examined node before the fold began. Though this is
more powerful than the catamorphism, in many cases it does not go far
enough: many useful functions are defined not just in terms of the
original argument to the function, but in terms of previous computed
values. The classic\footnote{Unfortunately, in this context I think
  ``classic'' can be read as ``hackneyed and unhelpful''. I dislike
  using \texttt{fib()} to teach recursion schemes, as the resulting
  implementations are both more complicated than a straightforward
  implementation and in no way indicative of the power that recursion
  schemes bring to the table. Throughout this series, I've done my
  damnedest to pick interesting, beautiful examples, lest the reader end
  up with the gravely mistaken takeaway that recursion schemes aren't
  useful for any real-world purpose.} example is the Fibonacci function,
the general case of which is defined in terms of two previous
invocations:

\begin{code}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
\end{code}

We could express this function using a catamorphism---though one of the
carrier values (\texttt{fib\ (n-1)}) would be preserved, as the
accumulator of our fold, we would need another explicit recursive call
to \texttt{cata} to determine the historical value of
\texttt{fib\ (n-2)}. This is a bummer, both in terms of
efficiency---we're recalculating values we've already calculated---and
in terms of beauty: a function so fundamental as \texttt{fib} deserves a
better implementation, especially given the expressive power of
recursion schemes.

The imperative programmers among us will have a solution to this
inefficiency: ``iterate!'', they will yell, or perhaps they will clamor
``introduce a cache!'' in a great and terrible voice. And it's true: we
could compute \texttt{fib} with a for-loop or by memoizing the recursive
call. But the former approach entails mutable state---a very big can of
worms to open for such a simple problem---and the latter leaves us
\href{https://twitter.com/importantshock/status/241173326846898176}{with
two problems}. Uustalu and Vene's histomorphism provides a way out: we
will \emph{preserve the history} of the values our fold computes, so
that further recursive calls to compute past values become unnecessary.
This style of recursion is called \emph{course-of-value recursion},
since we record the \emph{values} evaluated as our fold function
\emph{courses} through the structure.

Rather than operate on an \texttt{f\ a}, a data structure in the process
of being folded, we'll operate on a more sophisticated structure, so
that the argument to our fold function contains the history of all
applications of the fold itself. Instead of a just a carrier value
\texttt{a}, our \texttt{f} will contain a carrier value and a recursive,
unrollable record of past invocations, to wit:

\begin{code}
data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)
              }
\end{code}

We'll call this \texttt{Attr}, since it's an `attributed' version of
\texttt{Term}.

An \texttt{Attr\ f\ a} contains an \texttt{a}---a carrier value, storing
the in-progress value of the fold---as well as a fixed-point value
(analogous to \texttt{Term}) at each level of recursion. Thanks to the
fixed-point \texttt{hole} within the \texttt{f}, further \texttt{Attr}
items are preserved, each of which contains the shape of the folded
functor \texttt{f}. And within the \texttt{f} there lie further
\texttt{Attr} values, each of which contains a carrier value yielded by
\emph{their} application in their \texttt{attribute} slot. And those
\texttt{Attr} values in turn contain further \texttt{hole}s, which
contain the historical records pertaining to \emph{their} childrens'
history, and so on and so forth until the bottom of the data structure
has been reached. As such, the entire history of the fold is accessible
to us: the \texttt{holes} preserve the shape of the data structure
(which was lost during \texttt{cata}), and the \texttt{attribute} holds
the record of applying the fold to each entity in said data structure.

We have a word for preserving a record of the past, of
course---\emph{history}\footnote{A word with a rich pedigree---most
  directly from the Greek `ἱστορία', meaning \emph{a narration of what
  has been learned}, which in turn descended from `ἱστορέω', \emph{to
  learn through research}, and in turn from `ἵστωρ', meaning \emph{the
  one who knows} or \emph{the expert}--- a term commensurate with the
  first histories being passed from person to person orally. And the
  Greek root `ἱστο', according to the OED, can be translated as `web': a
  suitable metaphor for the structural web of values that the
  \texttt{Attr} type generates and preserves.}. A fold operation that
uses \texttt{Attr} to provide both an accumulator and a record of prior
invocations is known as a \emph{histomorphism}---a shape-changing
(\emph{morpho}) fold with access to its history (\emph{histo}).

Let's define the histomorphism. It will, like its cousins \texttt{cata}
and \texttt{para}, use an algebra for its fold function. But unlike the
F-algebra of \texttt{cata} or the R-algebra of \texttt{para}, we'll be
using an algebra that operates on an \texttt{Attr\ f\ a}, yielding an
\texttt{a} out of it. We call this a course-of-value algebra,
abbreviated to a \emph{CV-algebra}, and define a type alias for it, so
we end up with a more comprehensible type signature in the
histomorphism:

\begin{code}
type CVAlgebra f a = f (Attr f a) -> a
\end{code}

That is, a CV-algebra maps from a container \texttt{f} containing
children of type \texttt{Attr\ f\ a} (which in turn contain
\texttt{f\ (Attr\ f\ a)} children, as far down as is needed in the
nested structure), to a final result type \texttt{a}. The shape of the
folded structure and the history of its applications are all contained
in its \texttt{Attr} values: all you have to do is unroll the
\texttt{hole} value to go back one level in history and use
\texttt{attribute} to examine the stored value.

Our \texttt{histo} function will be similar to \texttt{cata} and
\texttt{para} at its heart. We start by unpacking the
\texttt{Term}---the initial argument must be a \texttt{Term} rather than
an \texttt{Attr}, since as we haven't started the fold yet we have no
value to fill in for \texttt{attribute}. We will then recurse, with
\texttt{fmap}, into the thus-revealed structure until we hit its root.
We then use the CV-algebra to build the value, starting at the root and
continuing upwards to the topmost leaf. These steps are analogous to how
we defined \texttt{cata} and \texttt{para}, so let's start defining it:

\begin{verbatim}
histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = out >>> fmap worker >>> h 
\end{verbatim}

But what type should the worker have? Well, we can ask GHC, thanks to
one of its most useful features\footnote{A feature taken wholesale, we
  must note, from dependently-typed languages like Agda and Idris.}---type
holes. By prepending an underscore to the use of \texttt{worker}, we can
allow the program compilation to continue as far as is
possible---however, when the compilation process has finished, GHC will
remind us where we used a type hole, and inform us of the type signature
it inferred for \texttt{\_worker}. (As a full-time Haskell programmer, I
use this feature nearly every day.)

\begin{verbatim}
histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = out >>> fmap _worker >>> h 
\end{verbatim}

Running this code in GHC yields the following type-hole message:

\begin{verbatim}
/Users/patrick/src/morphisms/src/Main.hs:14:24: error:
    • Found hole: ‘_worker’ with type :: Term f -> Attr f a
\end{verbatim}

Okay, that makes sense! We're operating on \texttt{Term\ f} values
(lifted into this context by the \texttt{fmap} within \texttt{histo}),
and we need to yield an \texttt{Attr\ f\ a}, so that the outside
\texttt{Term\ f} can be transformed into an \texttt{f\ (Attr\ f\ a)} and
then passed into the CV-algebra.

An \texttt{Attr\ f\ a}, as defined above, contains two values: a plain
\texttt{a} type, and a recursive \texttt{f\ (Attr\ f\ a)} hole. Given a
\texttt{Term\ f} and our ability to invoke both \texttt{histo} and
\texttt{worker} recursively, we can build the \texttt{Attr\ f\ a} we
need. Let's start by defining the skeleton of \texttt{worker}: given a
\texttt{Term\ f}, called \texttt{t}, it constructs an \texttt{Attr},
containing two fields.

\begin{verbatim}
worker t = Attr _ _
\end{verbatim}

The first field, the \texttt{a}, is yielded by recursing with
\texttt{histo} on the provided \texttt{Term}---easy enough. This is just
like the catamorphism---indeed, a catamorphism is a histomorphism that
ignores the provided history.

\begin{verbatim}
worker t = Attr (histo h term) _
\end{verbatim}

The second field's construction is more clever: we unwrap \texttt{term}
with the \texttt{out} function, which gives us an \texttt{f\ (Term\ f)}
out of a \texttt{Term\ f}. Since we don't know exactly what type
\texttt{f} is yet, we can't extract the contained \texttt{Term\ f}---but
we can operate on it, with \texttt{fmap}, provided by the
\texttt{Functor} constraint. So, to go from an \texttt{f\ (Term\ f)} to
an \texttt{f\ (Attr\ f\ a)}, we need a function of type
\texttt{Term\ f\ -\textgreater{}\ Attr\ f\ a}\ldots{} hang on, that's
just \texttt{worker} itself!

\begin{verbatim}
worker t = Attr (histo h term) (fmap worker (out t))
\end{verbatim}

This is the heart of \texttt{histo}`s elegance: it's 'doubly recursive',
in that its \texttt{worker} function invokes both \texttt{histo} and
\texttt{worker} itself.

Now we have a \texttt{histo} function that passes the typechecker:

\begin{verbatim}
histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = out >>> fmap worker >>> h where
    worker t = Attr (histo h t) (fmap worker (out t))
\end{verbatim}

However, this function does not share its subcomputations properly: each iteration of
\texttt{worker} recomputes, rather than reuses, all the nested \texttt{hole} values within
the constructed \texttt{Attr}. We can fix this by promoting \texttt{worker} to operate on
\texttt{Attr} values; by recursing with \texttt{fmap worker}, placing the input and output of
the CV-algebra in a tuple with \texttt{&&&}, and then unpacking the tuple into an \texttt{Attr},
we ensure that all the constructed \texttt{Attr} values share their subcomputations.

\begin{code}
histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = worker >>> attribute where
  worker = out >>> fmap worker >>> (h &&& id) >>> mkAttr
  mkAttr (a, b) = Attr a b
\end{code}

But what does this function \emph{mean}? We've filled in all
these type holes, and we have a working \texttt{histo} function, but why
does it work? Why does this preserve the history?

The answer lies in \texttt{worker}, in the \texttt{id} function that captures
and preserves the \texttt{Attr} the worker function is operating on. If we omitted that expression, we
would have a function equivalent to \texttt{cata}---one that throws all
its intermediate variables away while computing the result of a fold.
But our worker function ensures that the result computed at each stage is
not lost: as we flow, root-to-leaf, upwards through the data structure, we construct a
new \texttt{Attr} value, which in turn contains the previous result,
which itself preserves the result before that, and so on. Each step
yields an up-to-date snapshot of what we have computed in the past.

By \emph{not throwing out intermediate results}, and pairing these
intermediate results with the values used to calculate them, we
automatically generate \emph{and update} a cache for our fold.

Now, I may have used \texttt{fib} as an example of a course-of-value
recursive function, but I won't provide an example of using
\texttt{histo} to calculate the nth Fibonacci number (though it's a good
exercise). Let's solve a toy problem that's slightly more interesting,
one that histomorphisms make clear and pure, and one whose solution can
be generalized to all other problems of its ilk.

\subsection{C-C-C-Changes}\label{c-c-c-changes}

The
\href{https://en.wikipedia.org/wiki/Change-making_problem}{change-making
problem} is simple: given a monetary amount \texttt{N}, and a set of
denominations (penny, nickel, dime, \&c.), how many ways can you make
change for \texttt{N}? While it's possible to write a naïve recursive
solution for this problem, it becomes intolerably slow for large values
of \texttt{N}: each computation for \texttt{N} entails computing the
values for \texttt{N\ -\ 1}, and \texttt{N\ -\ 2}, and \texttt{N\ -\ 3},
and so forth: if we don't store these intermediate amounts in a cache,
we will waste our precious time on this earth. And, though this era may
be grim as all hell, slow algorithms are no way to pass the time.

We'll start by setting up a list of standard denominations. Feel free to
adjust this based on the denominational amounts of your country of
residence.

\begin{code}
type Cent = Int

coins :: [Cent]
coins = [50, 25, 10, 5, 1]

\end{code}

So our fundamental procedure is a function \texttt{change}, that takes a
cent amount and returns a count of how many ways we can make change for
said cent amount:

\begin{verbatim}
change :: Cent -> Int
\end{verbatim}

It is here where we hit our first serious roadblock. I asserted earlier
that the change-making problem, and all the other
\href{https://en.wikipedia.org/wiki/Knapsack_problem}{knapsack problems}
of its ilk, are soluble with a histomorphism---a cached fold over some
sort of data structure. But here we're dealing with\ldots{}
natural-number values. There are no lists, no vectors, no rose
trees---nothing mappable (that is to say, nothing with a
\texttt{Functor} instance) and therefore nothing to fold over. What are
we supposed to do?

All is not lost: we can fold over the natural numbers, just as we would
fold over a list. We just have to define the integers in an
unconventional, but simple, way: every natural number is either zero, or
1 + the previous. We'll call this formulation of the natural numbers
\texttt{Nat}--- the zero value will be \texttt{Zero}\footnote{Natch.},
and the notion of the subsequent number \texttt{Next}. Put another way,
we need to encode
\href{https://en.wikipedia.org/wiki/Peano_axioms}{Peano numerals} in
Haskell\footnote{Keen-eyed readers will note that this data type is
  isomorphic to the \texttt{Maybe} type provided by the Prelude. We
  could've just used that, but I wanted to make the numeric nature of
  this structure as clear as possible.}.

\begin{code}
data Nat a
    = Zero
    | Next a
    deriving Functor
\end{code}

We use \texttt{Term} to parameterize \texttt{Nat} in terms of
itself---that is to say, given \texttt{Term}, we can stuff a
\texttt{Nat} into it so as to represent an arbitrarily-nested hierarchy
of contained \texttt{Nat}s, and thus represent all the natural numbers:

\begin{verbatim}
one, two, three :: Term Nat
one   = In (Next (In Zero))
two   = In (Next one)
three = In (Next two)
\end{verbatim}

For convenience's sake, we'll define functions that convert from
standard \texttt{Int} values to foldable \texttt{Term\ Nat}s, and vice
versa. Again, these do not look particularly efficient, but please give
me the benefit of the doubt.

\begin{code}
-- Convert from a natural number to its foldable equivalent, and vice versa.
expand :: Int -> Term Nat
expand 0 = In Zero
expand n = In (Next (expand (n - 1)))

compress :: Nat (Attr Nat a) -> Int
compress Zero              = 0
compress (Next (Attr _ x)) = 1 + compress x
\end{code}

While this is, at a glance, obviously less-efficient than using
integers, it's not as bad as it seems. We only have three operations:
increment, converting from zero, and converting to zero. Restricting our
operations to these---rather than writing our own code for addition or
subtraction, both of which are linear-time over the Peano
numerals---means that operations on our \texttt{Term\ Nat} types are
almost the same as hardware-time costs, barring GHC-specific operations.
As such, the expressivity we yield with our foldable numbers is well
worth the very slight costs.

Given an amount (\texttt{amt}), we solve the change-making problem by
converting that amount to a \texttt{Term\ Nat} with \texttt{expand},
then invoking \texttt{histo} on it with a provided CV-algebra---let's
call it \texttt{go}. We'll define it in a where-clause below.

\begin{code}
change :: Cent -> Int
change amt = histo go (expand amt) where
\end{code}

Since we're operating on foldable natural values (\texttt{Nat}) and
ultimately yielding an integral result (the number of ways it is
possible to make change for a given \texttt{Nat}), we know that our
CV-algebra will have as its carrier functor \texttt{Nat} and its result
type \texttt{Int}.

\begin{code}
  -- equivalent to Nat (Attr Nat Int) -> Int  
  go :: Nat (Attr Nat Int) -> Int
\end{code}

Because \texttt{histo} applies its algebra from leaf-to-root, it starts
at the deepest nested position in the \texttt{Term\ Nat}---that is to
say, \texttt{Zero}. We know that there's only one way to make change for
zero coins---by giving zero coins back---so we encode our base case by
explicitly matching on a Zero and returning 1.

\begin{code}
  go Zero = 1
\end{code}

Now comes the interesting part---we have to match on \texttt{Next}.
Contained in that \texttt{Next} value will be an \texttt{Attr\ Nat\ Int}
(which we'll refer to as \texttt{attr}), containing the value yielded
from applying \texttt{go} to the previous \texttt{Nat}ural number. Since
we'll need to feed this function into \texttt{compress} to perform
actual numeric operations on it (since we did not write the requisite
boilerplate to make \texttt{Nat} an instance of the \texttt{Num}
typeclass\footnote{There is no reason why we couldn't do this---I just
  chose to omit it for the sake of brevity.}), we'll use an @-pattern to
capture it under the name \texttt{curr}.

\begin{code}
  go curr@(Next attr) = let
\end{code}

Because we need to find out what numeric amounts (from \texttt{coins})
are valid change-components for \texttt{curr}, we have to get an
\texttt{Int} out of \texttt{curr}. We'll call this value \texttt{given},
since it's our given amount.

\begin{code}
    given               = compress curr
\end{code}

Now we have to look at each value of the \texttt{coins} list. Any values
greater than \texttt{given} are right out: you can't use a quarter to
make change for a dime, obviously.

\begin{code}
    validCoins          = filter (<= given) coins
\end{code}

Now we subtract the \texttt{given} amount from each element of
\texttt{validCoins}. This list represents, for each coin in
\texttt{validCoins}, how much change we have remaining after using that
coin to make change for \texttt{given}---if \texttt{given} were equal to
10, the list would be \texttt{{[}9,\ 5,\ 0{]}}.

\begin{code}
    remaining           = map (given -) validCoins
\end{code}

Now we partition this \texttt{remaining} list into two sublists: the items
equal to zero and those that are not. We don't need to consult the lookup table
for the items that are zero, obviously, but we need to do so for the others.

\begin{code}
    (zeroes, toProcess) = partition (== 0) remaining
\end{code}


Given each number in \texttt{toProcess}, we have to consider how many ways we could make
change out of that number---but, since we know that that we've already
calculated that result, because it's by definition less than
\texttt{given}! So all we have to do is look up the cached result in our
\texttt{attr}. (We'll implement the \texttt{lookup} function later
on---it is two lines of code.) We'll add all these cached results
together with \texttt{sum}.

\begin{code}
    results             = sum (map (lookup attr) toProcess)
\end{code}

Then all that's left to do is add \texttt{zeroCount} and \texttt{others}
together.

\begin{code}
    in length zeroes + results
\end{code}

Let's take a look at what we've written so far.

\begin{verbatim}
change :: Cent -> Int
change amt = histo go (expand amt) where
  go :: Nat (Attr Nat Int) -> Int
  go Zero = 1
  go curr@(Next attr) = let
    given               = compress curr
    validCoins          = filter (<= given) coins
    remaining           = map (given -) validCoins
    (zeroes, toProcess) = partition (== 0) remaining
    results             = sum (map (lookup attr) toProcess)
    in length zeroes + results
\end{verbatim}

Wow. This is pretty incredible. Not only do we have a simple, pure,
concise, and performant solution to the change-making problem, but the
caching is \emph{implicit}: we don't have to update the cache ourselves,
because \texttt{histo} does it for us. We've stripped away the artifacts
required to solve this problem efficiently and zeroed in on the essence
of the problem. This is remarkable.

I told you I would show you how to look up the cached values, and indeed
I will do so now. An \texttt{Attr\ Nat\ a} is essentially a nonempty
list: if we could pluck the most-final \texttt{Attr\ Nat\ a} after
\texttt{change} has finished executing, we would see the value of
\texttt{change\ 0} stored inside the first \texttt{attribute} value, the
value of \texttt{change\ 1} stored inside the \texttt{attribute} within
the first attribute's \texttt{hole}, and the value for
\texttt{change\ 2} inside that further \texttt{hole}. So, given an index
parameter \texttt{n}, we return the \texttt{attribute} if \texttt{n} is
0, and we recurse inside the \texttt{hole} if not, with
\texttt{n\ -\ 1}.

\begin{code}
lookup :: Attr Nat a -> Int -> a
lookup cache 0 = attribute cache
lookup cache n = lookup inner (n - 1) where (Next inner) = hole cache
\end{code}

\subsection{A Shape-Shifting Cache}\label{a-shape-shifting-cache}

Something crucial to note is that the fixed-point accumulator---the
\texttt{f\ (Attr\ f\ a)} parameter to our CV-algebra---\emph{changes
shape} based on the functor \texttt{f} contained therein. Given an
inductive functor \texttt{Nat} that defines the natural numbers,
\texttt{Nat\ (Attr\ Nat\ a)} is isomorphic to \texttt{{[}{]}}, the
ordinary linked list: a \texttt{Zero} is the empty list, and a
\texttt{Next} that contains a value (stored in \texttt{Attr}'s
\texttt{attribute} field) and a pointer to the next element of the list
(stored in the \texttt{hole\ ::\ Nat\ (Attr\ Nat\ a))} field in the
given \texttt{Attr}). This is why our implementation of \texttt{lookup}
is isomorphic to an implementation of \texttt{!!} over
\texttt{{[}{]}}---because they're the same thing.

But what if we use a different \texttt{Functor} inside an \texttt{Attr}?
Well, then the shape of the resulting \texttt{Attr} changes. If we
provide the list type---\texttt{{[}{]}}---we yield
\texttt{Attr\ {[}{]}\ a}, which is isomorphic to a rose tree---in
Haskell terms, a \texttt{Tree\ a}. If we use \texttt{Either\ b}, then
\texttt{Attr\ (Either\ b)\ a} is a nonempty list of computational steps,
terminating in some \texttt{b} value. \texttt{Attr} is more than an
``attributed \texttt{Term}''---it is an \emph{adaptive cache} for a fold
over \emph{any type of data structure}. And that is truly wild.

\subsection{Obsoleting Old
Definitions}\label{obsoleting-old-definitions}

As with \texttt{para}, the increased power of \texttt{histo} allows us
to express \texttt{cata} with new vocabulary. Every F-algebra can be
converted into a CV-algebra---all that's needed is to ignore the
\texttt{hole} values in the contained Functor \texttt{f}. We do this by
mapping \texttt{attribute} over the functor before passing it to the
F-algebra, throwing away the history contained in \texttt{hole}.

\begin{code}
cata :: Functor f => Algebra f a -> Term f -> a
cata f = histo (fmap attribute >>> f)
\end{code}

Similarly, we can express \texttt{para} with \texttt{histo}, except
instead of just fmapping with \texttt{attribute} we need to do a little
syntactic juggling to convert an \texttt{f\ (Attr\ f\ a)} into an
\texttt{f\ (Term\ f,\ a)}. (Such juggling is why papers tend to use
banana-bracket notation: implementing this in an actual programming
language often requires syntactic noise such as this.)

\begin{code}
para :: Functor f => RAlgebra f a -> Term f -> a
para f = histo (fmap worker >>> f) where
  worker (Attr a h) = (In (fmap (worker >>> fst) h), a)
\end{code}

\subsection{Controlling the Future with
Futumorphisms}\label{controlling-the-future-with-futumorphisms}

Throughout this series, we can derive unfolds from a corresponding fold
by ``reversing the arrows''---viz., finding the function dual to the
fold in question. And the same holds true for histomorphisms---the dual
is very powerful. But, to find the dual of \texttt{histo}, we must first
find the dual of \texttt{Attr}.

Whereas our \texttt{Attr} structure held both an \texttt{a} and a
recursive \texttt{f\ (Attr\ f\ a)} structure, its
dual---\texttt{CoAttr}---holds \emph{either} an \texttt{a} value---we'll
call that \texttt{Automatic}---or a recursive \texttt{f\ (CoAttr\ f\ a)}
value, which we'll call \texttt{Manual}. (Put another way, since
\texttt{Attr} was a product type, its dual is a sum type.) The
definition follows:

\begin{code}
data CoAttr f a
  = Automatic a
  | Manual (f (CoAttr f a))
\end{code}

And the dual of a CV-algebra is a CV-coalgebra:

\begin{code}
type CVCoalgebra f a = a -> f (CoAttr f a)
\end{code}

So why call these \texttt{Automatic} and \texttt{Manual}? It's
simple---returning a \texttt{Manual} value from our CV-coalgebra means
that we specify manually how the unfold should proceed at this level, which allows us to unfold
more than one level at a time into the future. By contrast, returning a
\texttt{Automatic} value tells the unfold to continue automatically at this level. This
is why we call them \emph{futu}morphisms---our CV-coalgebra allows us to
determine the \emph{future} of the unfold. (The term `futumorphism' is
etymologically dubious, since the `futu-' prefix is Latin and the
`-morpho' suffix is Greek, but there are many other examples of such
dubious words: `television', `automobile', and `monolingual', to name
but a few.)

Like its predecessor unfolds \texttt{ana} and \texttt{apo}, the
futumorphism will take a coalgebra, a seed value \texttt{a}, and produce
a term \texttt{f}:

\begin{verbatim}
futu :: Functor f => CVCoalgebra f a -> a -> Term f
\end{verbatim}

We derived the anamorphism and apomorphism by reversing the arrows in
the definitions of \texttt{cata} and \texttt{para}. The same technique
applies here---\texttt{\textgreater{}\textgreater{}\textgreater{}}
becomes \texttt{\textless{}\textless{}\textless{}}, and \texttt{In}
becomes \texttt{out}. And as previously, we use a type hole to derive
the needed signature of the helper function.

\begin{verbatim}
futu :: Functor f => CVCoalgebra f a -> a -> Term f
futu f = In <<< fmap _worker <<< f
\end{verbatim}

\begin{verbatim}
/Users/patrick/src/morphisms/src/Main.hs:28:32: error:
    • Found hole: ‘_worker’ with type :: CoAttr f a -> Term f
\end{verbatim}

This also makes sense! The worker function we used in \texttt{histo} was
of type \texttt{Term\ f\ -\textgreater{}\ Attr\ f\ a}---by reversing the
arrows in this worker and changing \texttt{Attr} to \texttt{CoAttr},
we've derived the function we need to define \texttt{futu}. And its
definition is straightforward:

\begin{code}
futu :: Functor f => CVCoalgebra f a -> a -> Term f
futu f = In <<< fmap worker <<< f where
    worker (Automatic a) = futu f a        -- continue through this level
    worker (Manual g) = In (fmap worker g) -- omit folding this level,
                                           -- delegating to the worker
                                           -- to perform any needed 
                                           -- unfolds later on.
\end{code}

When we encounter a plain \texttt{Continue} value, we continue recursing
into it, perpetuating the unfold operation. When we encounter a
\texttt{Stop} value, we run one more iteration on the top layer of the
in-progress fold (transforming its children from \texttt{Coattr\ f\ a}
values into \texttt{Term\ f} values by recursively invoking
\texttt{worker}), then wrap the whole item up with an \texttt{In}
constructor and return a final value. The product of this nested
invocation of \texttt{worker} is then similarly passed to the
\texttt{In} constructor to wrap it up in a fixpoint, then returned as
the final output value of \texttt{futu}.

What differentiates this from \texttt{apo}---which, if you recall, used
an \texttt{Either} type to determine whether or not to continue the
unfold---is that we can specify, \emph{in each field of the functor f},
whether we want to continue the unfold or not. \texttt{apo} gave us a
binary switch---either stop the unfold with a \texttt{Left} or keep
going with a \texttt{Right}. \texttt{futu}, by contrast, lets us build
out as many layers at a time as we desire, giving us the freedom to
manually specify the shape of the structure or relegate its shape to
future invocations of the unfold.

This is an interesting way to encode unfolds! A CV-coalgebra that always
returns a \texttt{Continue} value will loop infinitely, such as the
unfold that generates all natural numbers. This means that we can tell,
visually, whether our unfold is infinite or terminating.

``But Patrick,'' you might say, ``this looks like a cellular
automaton.'' And you would be right---CV-coalgebras describe tree
automata. And in turn, coalgebras describe finite-state automata, and
R-coalgebras describe stream automata. We'll use this fact to define an
example CV-coalgebra, one that grows\footnote{which brings an amusing
  literalism to the term `seed value'} random plant life.

\subsubsection{Horticulture with
Futumorphisms}\label{horticulture-with-futumorphisms}

Let's start by defining the various parts of a plant.

\begin{code}
data Plant a
  = Root a     -- every plant starts here
  | Stalk a    -- and continues upwards
  | Fork a a a -- but can trifurcate at any moment
  | Bloom      -- eventually terminating in a flower
    deriving (Show, Functor)
\end{code}

Let's define a few rules for how a plant is generated. (These should, as
I mentioned above, remind us of the rules for tree automata.)

\begin{verbatim}
1. Plants begin at the ground. 
2. Every plant has a maximum height of 10.
3. Plants choose randomly whether to fork, grow, or bloom.
4. Every fork will contain one immediate bloom and two further stems.
\end{verbatim}

Rather than using integers to decide what action to take, which can get
obscure very quickly, let's define another sum type, one that determines
the next step in the growth of the plant.

\begin{code}
data Action
  = Flower  -- stop growing now
  | Upwards -- grow up with a Stalk
  | Branch  -- grow up with a Fork
\end{code}

Because we need to keep track of the total height and a random number
generator to provide randomness, we'll unfold using a data type
containing an \texttt{Int} to track the height and a \texttt{StdGen}
generator from \texttt{System.Random}.

\begin{code}
data Seed = Seed
    { height :: Int
    , rng    :: Random.StdGen
    }
\end{code}

We'll define a function \texttt{grow} that takes a seed and returns both
an randomly-chosen action and two new seeds. We'll generate an action by choosing a
random number from 1 to 5: if it's 1 then we'll choose to
\texttt{Flower}, if it's 2 we'll choose to \texttt{Branch}, and
otherwise we'll choose to grow \texttt{Upwards}. (Feel free to change
these values around and see the difference in the generated plants.) The
\texttt{Int} determining the height of the plant is incremented every
time \texttt{grow} is called. 

\begin{code}
grow :: Seed -> (Action, Seed, Seed)
grow seed@(Seed h rand) = (choose choice, left { height = h + 1}, right { height = h + 1})
  where (choice, _) = Random.randomR (1 :: Int, 5) rand
        (leftR, rightR) = Random.split rand
        left = Seed h leftR
        right = Seed h rightR
        choose 1 = Flower
        choose 2 = Branch
        choose _ = Upwards
\end{code}

And now we'll define a CV-coalgebra, one that takes a \texttt{Seed} and
returns a \texttt{Plant} containing a \texttt{CoAttr} value.

\begin{code}
sow :: CVCoalgebra Plant Seed
\end{code}

The definition falls out rather quickly. We'll start by growing a new
seed, then examining the current height of the plant:

And now we'll define a CV-coalgebra, one that takes a \texttt{Seed} and
returns a \texttt{Plant} containing a \texttt{CoAttr} value.

\begin{verbatim}
sow :: CVCoalgebra Plant Seed
\end{verbatim}

The definition falls out rather quickly. We'll start by growing a new
seed, then examining the current height of the plant:

\begin{verbatim}
sow seed =
  let (action, next) = grow seed
  in case (height seed) of
\end{verbatim}

Since we'll start with a height value of 0, we'll begin by generating a
root (rule 1). Because we want to immediately continue onwards with the
unfold, we pass a \texttt{Continue} into this \texttt{Root}, giving it
the subsequent seed (so that we get a new RNG value).

\begin{verbatim}
   0 -> Root (Continue next)
\end{verbatim}

Rule 2 means that we must cap the height of the plant at 10. So let's do
that:

\begin{verbatim}
   10 -> Bloom
\end{verbatim}

Otherwise, the height is immaterial. We must consult the \texttt{action}
variable to know what to do next.

\begin{verbatim}
   _  -> case action of
\end{verbatim}

If the action is to \texttt{Flower}, then we again return a
\texttt{Bloom}.

\begin{verbatim}
      Flower -> Bloom
\end{verbatim}

If it's to grow \texttt{Upwards}, then we return a \texttt{Stalk}, with
a contained \texttt{Continue} value to continue our fold at the top of
that \texttt{Stalk}:

\begin{verbatim}
      Upwards -> Stalk (Continue next)
\end{verbatim}

And now we handle the \texttt{Branch} case. Our rules dictate that one
of the branches will stop immediately, and the other two will continue,
after a given length of \texttt{Stalk}. So we return a \texttt{Fork}
with one \texttt{Stop} and two \texttt{Continues}.

\begin{verbatim}
      Branch  -> Fork -- grow a stalk then continue the fold
                     (Stop (Stalk (Continue next)))
                     -- halt immediately
                     (Stop Bloom)
                      -- again, grow a stalk and continue
                     (Stop (Stalk (Continue next)))
\end{verbatim}

Note how, even though we specify the construction of a \texttt{Stalk} in
the first and third slots, we allow the fold to \texttt{Continue}
afterwards. This is the power of the futumorphism: we can choose the
future of our folds, layer by layer. This is not possible with an
anamorphism or apomorphism.

Here's our full \texttt{sow} function, rewritten slightly to use one
\texttt{case} statement:

\begin{code}
sow seed =
  let (action, left, right) = grow seed
  in case (action, height seed) of
    (_, 0)       -> Root (Automatic left)
    (_, 10)      -> Bloom
    (Flower, _)  -> Bloom
    (Upwards, _) -> Stalk (Automatic right)
    (Branch, _)  -> Fork (Manual (Stalk (Automatic left)))
                         (Manual Bloom)
                         (Manual (Stalk (Automatic right)))
\end{code}

\ignore{
\begin{code}
-- I can't find the original implementation I had of this function. I will
-- do it more properly later.  
render :: Algebra Plant Box
render Bloom    = "8"
render (Root a) = vcat center1 ["X", a]
\end{code}
}

This is pretty remarkable. We've encoded a complex set of rules, one
that involves both nondeterminism and strict layout requirements, into
one CV-coalgebra, and it took just eleven lines of code. No mutable
state is involved, no manual accumulation is required---the entire
representation of this automaton can be reduced to one pure function.

Now, in our \texttt{main} function, we can grab an RNG from the global
state, and call \texttt{futu} to generate a \texttt{Term\ Plant}.

\begin{verbatim}
main :: IO ()
main = do
  rnd <- newStdGen
  let ourPlant :: Term Plant
      ourPlant = futu sow (Seed 0 rnd)
\end{verbatim}

Using a rendering function (which I have omitted for brevity's sake,
though you can be assured that it is implemented using \texttt{cata}
rather than explicit recursion), we can draw a picture of the plant
we've just generated, with little flowers.

\begin{verbatim}
⚘
| ⚘     ⚘          ⚘
|⚘|     |          |
└─┘     |         |
 |      |          |       ⚘
 |  ⚘   |          |       |
 └─────┘          |   ⚘   | 
    |              └──────┘ 
    |        ⚘        |
    └───────────────┘
             |
             _
\end{verbatim}

Admittedly, the vaguaries of
\href{https://en.wikipedia.org/wiki/Code_page_437}{code page 437} leave
us with a somewhat unaesthetic result---but a nicer representation of
\texttt{Plant}, perhaps using
\href{https://hackage.haskell.org/package/gloss}{gloss} or
\href{https://hackage.haskell.org/package/Rasterific}{Rasterific}, is
left as an exercise for the reader.

One final detail: just as we can use an apomorphism to express an
anamorphism, we can express anamorphisms and apomorphisms with
futumorphisms:

\begin{code}
ana :: (Functor f) => Coalgebra f a -> a -> Term f
ana f = futu (fmap Automatic <<< f)

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo f = futu (fmap (either termToCoattr Automatic) <<< f)
  where termToCoattr = Manual <<< fmap termToCoattr <<< out
\end{code}

\subsubsection{My God, It's Full of
Comonads}\label{my-god-its-full-of-comonads}

Now we know what histomorphisms and futumorphisms are. Histomorphisms
are folds that allow us to query any previous result we've computed, and
futumorphisms are unfolds that allow us to determine the future course
of the unfold, multiple levels at a time. But, as is so often the case
with recursion schemes, these definitions touch on something deeper and
more fundamental.

Here's the kicker: our above \texttt{CoAttr} definition is equivalent to
the \texttt{Free} monad, and \texttt{Attr} (being dual to
\texttt{CoAttr}) is the \texttt{Cofree} comonad.

We usually represent \texttt{Free}, aka \texttt{CoAttr}, as two
constructors, one for pure values and one for effectful, impure values:

\begin{verbatim}
data Free f a
    = Pure a
    | Impure (f (Free f a))
\end{verbatim}

And we usually represent the cofree comonad with an infix constructor,
since the cofree comonad is at its heart a glorified tuple:

\begin{verbatim}
data Cofree f a = a :< (f (Cofree f a))
\end{verbatim}

The various packages in the Haskell ecosystem implement \texttt{cata}
and \texttt{para} in much the same way, but the same is not true of
\texttt{histo} and \texttt{futu}. Edward Kmett's
\href{https://hackage.haskell.org/package/recursion-schemes}{recursion-schemes}
package uses these definitions of \texttt{Free} and \texttt{Cofree}
(from the \href{https://hackage.haskell.org/package/free}{free}
package).
\href{https://hackage.haskell.org/package/fixplate}{\texttt{fixplate}}
uses a different definition of \texttt{Attr}: rather than being a data
type in and of itself, it is defined as a \texttt{Term} over a
more-general \texttt{Ann} type.
\href{https://hackage.haskell.org/package/compdata}{\texttt{compdata}}'s
is slightly more complicated, as it leverages other typeclasses
\texttt{compdata} provides to define attributes on nodes, but is at its
heart the same thing. Each is equivalent.

The free monad, and its cofree comonad dual, lie at the heart of some of
the most fascinating constructions in functional programming. I have
neither the space nor the qualifications to provide a meaningful
explanation of them, but I can enthusiastically recommend
\href{https://twitter.com/GabrielG439}{Gabriel Gonzales}'s blog post on
\href{http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html}{free
monads}, \href{https://twitter.com/sigfpe}{Dan Piponi}'s post on the
\href{http://blog.sigfpe.com/2014/05/cofree-meets-free.html}{cofree
comonad}, and (of course) Oleg Kiselyov's
\href{http://okmij.org/ftp/Computation/free-monad.html}{groundbreaking
work} on the free and freer monads. But I think the fact that, as we
explore as fundamental a construct as recursion, we encounter another
similarly fundamental concept of the free monad, provide an argument for
the beauty and unity of the category-theoretical approach to functional
programming that is far more compelling than any I could ever make
myself.

I'd like to thank Rob Rix, who was essential to this work's completion,
and Colin Barrett, who has been an invaluable resource on the many
occasions when I find myself stuck. I'd also like to thank Manuel
Chakaravarty, who has done this entire series a great favor in checking
it for accuracy, and Jeanine Adkisson, who found some outrageous bugs in
the provided futumorphism. Greg Pfiel, Scott Vokes, and Josh Bohde also
provided valuable feedback on drafts of this post. Mark Needham, Ian Griffiths,
and Bryan Grounds found important bugs in the first published version of this
post; I owe them a debt of gratitude. Next time, we'll
explore one of the most compelling reasons to use recursion
schemes---the laws that they follow---and after that, we'll discuss the
constructs derived from combining unfolds with folds: the hylomorphism
and the chronomorphism.

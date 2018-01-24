\long\def\ignore{}

\ignore{
\begin{code}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Part4Point5 where

import Numeric.Natural

\end{code}
}

In an effort to publish more than one blog post a year, I've decided to write about smaller topics.
Today I'm going to talk about the notion of a `base functor', and how the popular
\href{https://hackage.haskell.org/package/recursion-schemes}{recursion-schemes} library uses base functors to
make recursion schemes more elegant and ergonomic in practice.

\subsubsection{Repeating Ourselves}\label{repeating-ourselves}

Throughout this series of posts, we've seen the pattern of parameterizing our data types in terms of a recursive type variable. In the first installment, we went from this:

\begin{verbatim}
data Expr
  = Index Expr Expr
  | Call Expr [Expr]
  | Unary String Expr
  | Binary Expr String Expr
  | Paren Expr
  | Literal Int
  deriving (Show, Eq)
\end{verbatim}

to this—the same data type, but of kind\footnote{If you're not familiar with the notion of a `kind', you can
  think (loosely) about the kind of a data type \texttt{t} as a measure of how many arguments it takes. Our \texttt{Expr}
  type takes no arguments, so its kind is "star": \texttt{*}. A data type that takes one argument has kind `star
  to star', \texttt{* -> *}. A data type that takes three arguments, like \texttt{Either}, has
  kind \texttt{* -> * -> *}. The high-level description of a kind is `the type of a type', but you can think
  about them as merely providing information as to the parameters taken, if any, by a data type. (The \texttt{:k})
  directive in GHCi provides information on the kind of any type or typeclass you provide.} \texttt{* -> *}, with all recursive
occurrences of the \texttt{Expr} type replaced with the type variable \texttt{a}:

\begin{code}
data ExprF a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Int
  deriving (Show, Eq, Functor)
\end{code}

We then used the \texttt{Term} trick, a Y-combinator for the type system, to parameterize \texttt{ExprF} in terms of itself, yielding a data type equivalent to the original \texttt{Expr} but that can be mapped over with \texttt{fmap} and folded with \texttt{cata}:

\begin{code}
newtype Term f = In { out :: f (Term f) }

type Expr = ExprF (Term ExprF)
\end{code}

Similarly, in part 4 we represented the natural numbers with a data type of kind \texttt{* -> *}:

\begin{code}
data Nat a
    = Zero
    | Next a
    deriving Functor
\end{code}

And we represented the \texttt{Plant} type we grow with another such data type:

\begin{code}
data Plant a
  = Root a
  | Stalk a
  | Fork a a a
  | Bloom
\end{code}

This is a good and fluent way to build data types. However, it has its disadvantages.

Consider the humble linked list, containing values of type \texttt{a}: \texttt{[a]}.

\begin{verbatim}
infixr 5 :
data [] a = a : [a]
          | []
\end{verbatim}

Certainly \texttt{cata}, the fundamental right-fold operation, should be able to support folding over this structure: after all, it's of kind \texttt{* -> *}. But if we apply \emph{Term} to this data type, we run into trouble quite quickly: we \emph{lose the ability to store elements in a list}. The type variable \texttt{a} can only hold nested \texttt{Term a} values: there is no place we can actually store list elements. This is, as you can tell, not particularly useful.

Now, of course, we can do our standard song-and-dance routine, converting \texttt{[a]} into a type where its recursive element is replaced by a new type variable to represent recursive occurrences (the \texttt{[a]} in the \texttt{(:)} constructor:)

\begin{code}
data ListF a b
  = Cons a b
  | Nil
    deriving (Show, Eq, Functor)
\end{code}

This looks a little different from our prior examples—it's of kind \texttt{* -> * -> *}, not \texttt{* -> *}—but the principle is the same. We add a new type variable, here called \texttt{b}, to represent recursive occurrences of the \texttt{List} type. And now we can fold over a list:

\begin{verbatim}
listSum :: Num a => Algebra (ListF a) a
listSum (Cons a b) = a + b
listSum Nil = 0
\end{verbatim}

But this is clumsy. There's something fundamentally wrong if we have to write a replacement for the venerable \texttt{[]} type. Recursion schemes are meant to be elegant—manually creating alternate implementations for every recursive data type doesn't strike me as particularly elegant. But I am, thankfully, not alone in feeling this way—the literature contains many ways to work around this. I'll focus on the solution provided by the \href{https://hackage.haskell.org/package/recursion-schemes}{recursion-schemes} package.

\subsubsection{Base Functors to the Rescue}\label{base-functors}

The first thing you see when you open up the \texttt{recursion-schemes} documentation is the following type family declaration:

This understandably intimidates a number of people, especially given its lack of documentation. But this is less forbidding then it appears, and the added mental overhead is worth it—the presence of the \texttt{Base} type family is one of the things that sets \texttt{recursion-schemes} apart from its competition in terms of ease-of-use.

The purpose of the \texttt{Base} type family is to tie together a standard Haskell data type—such as our original \texttt{Expr} formulation, or the humble \texttt{[]} list—with its parameterized, "base" representation. Its definition is very concise:

\begin{code}
type family Base t :: * -> *
\end{code}

While a full-fledged tutorial on type families is beyond the scope of this post (the best such reference is the \href{https://wiki.haskell.org/GHC/Type_families}{GHC wiki}), we can think of type families as a way to write functions on the type-level. If we were to declare a type family, and an instance of this family (analogous to an instance of a typeclass):

\begin{verbatim}
type family Something t

type instance Something Foo = Bar
\end{verbatim}

then anywhere we encounter the invocation \texttt{Something Foo} the GHC type system will resolve that to \texttt{Bar}. While this may seem unnecessary—if you mean \texttt{Bar}, why not just write \texttt{Bar}?—it provides us a rich facility with which to \emph{associate} a type with another.

Look at the definition of \texttt{Base} again. The kind signature there indicates that, whenever you pass in a concrete type as the variable \texttt{t}, you will yield a data type parameterized with one additional variable. This corresponds to our experience with \texttt{ExprF} and \texttt{List}: \texttt{Expr} went from kind \texttt{*} to \texttt{* -> *}, and \texttt{[a]} went from kind \texttt{* -> *} to \texttt{* -> * -> *}.

The \texttt{Base} type family doesn't tell us much on our own. The most illustrative path open to us is to look at an instance declared with \texttt{Base}.

\begin{code}
type instance Base [a] = ListF a
\end{code}

This declares a type family instance. Anywhere we mean to read \texttt{ListF a}, we can write \texttt{Base [a]}. This provides important organizational meaning: there is only one sensible parameterized implementation for any recursive type, and thus (thanks to Haskell's support for type families) there is only one implementation of \texttt{Base a} for any given type \texttt{a}.

This isn't particularly exciting on its own. The real fireworks start here, with the definition (simplified here for pedagogical purposes) of the \texttt{Recursive} typeclass.

\begin{code}
class (Functor (Base t)) => Recursive t where
  project :: t -> Base t t
  cata    :: (Base t a -> a) -> t -> a
\end{code}

The \emph{Recursive} typeclass is similar to the \texttt{Foldable} typeclass\footnote{As you can see
  by the name of its containing module, \texttt{Data.Functor.Foldable}. This class was originally called
  \texttt{Foldable}, but the presence of \texttt{Foldable} in the standard library made the duplication
  unsupportable, and it was changed to \texttt{Recursive}}. The analogy holds: if we define a
\texttt{Recursive} instance for our data type \texttt{t}, whether that's \texttt{Expr} or \texttt{[a]} or
anything else, we can fold over this type. However, instead of providing a simple fold,
we provide two functions: a \texttt{project} function that takes a type \texttt{t} and returns the same
type but transformed into its \texttt{Base} form, and a \texttt{cata} function that, given an algebra
from \texttt{Base t a} to \texttt{a} (to wit: \texttt{Base t a -> a}), and an initial \texttt{t} value
over which to fold, yields us a folded \texttt{a} value.

This is, at first glance, more unwieldy than our formulation of \texttt{cata}, which only involved
a \texttt{Functor} constraint, and nothing related to \texttt{Base} functors or a new \texttt{Recursive}
type:

\begin{verbatim}
cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f
\end{verbatim}

But, critically, this formulation of \texttt{cata} forces us to work with \texttt{Term List} rather than
the simple \texttt{[a]}. However, \texttt{Recursive} allows us to work with ordinary data types: rather
than provide a \texttt{Term t} to \texttt{cata}, we give an ordinary \texttt{t}. \texttt{Recursive} instances
\emph{use the \texttt{project} function to transform the provided type into a parameterized one}, then pass
that transformed data type to the \texttt{cata} function. As such, we can now use \texttt{cata} to fold
over an ordinary list, without having to wrap it in \texttt{Term}s and \texttt{Cons} values.

\begin{code}
-- yields 10
sumList :: Num a => [a] -> a
sumList = cata go where
  go Nil = 0
  go (Cons a acc) = a + acc
\end{code}

\texttt{Recursive} has further tricks up its sleeve. Thanks to a \texttt{MINIMAL} pragma, the
\texttt{Recursive} class only needs an implementation of \texttt{project} to implement \texttt{Recursive}
fully—we still get a universal \texttt{cata} function for free. The implementation looks like this, if
you're interested:

\begin{verbatim}
class Functor (Base t) => Recursive t where
  project :: t -> Base t t

  cata :: (Base t a -> a) -- ^ a (Base t)-algebra
       -> t               -- ^ fixed point
       -> a               -- ^ result
  cata f = c where c = f . fmap c . project
\end{verbatim}

Note that the \texttt{cata} function is given a default definition. If you had some magic data type
that admitted a faster \texttt{cata} than the default implementation, you could override it—however,
I struggle to think of a type which would admit a custom \texttt{cata}.

Contained inside the \texttt{Recursive} class are other useful folds—\texttt{para}, the paramorphism,
which we discussed in part 3, and ones we haven't covered yet—the generalized paramorphism \texttt{gpara}
and Fokkinga's prepromorphism \texttt{prepro}. (We will discuss these in a future installment).

Note that the Base type is constrained in Recursive instances: \texttt{t} must have a \texttt{Base} instance,
and the \texttt{Base} instance for \texttt{t} must be a \texttt{Functor}. Since \texttt{cata} is defined
in terms of a recursive invocation with \texttt{fmap}, we need a useful \texttt{fmap} function over any
\texttt{Base} instance to have a \texttt{cata} that typechecks.

Thanks to the \texttt{Recursive} typeclass, we can deal in simple data types—\texttt{[a]} rather than
\texttt{ListF}, \texttt{Expr} rather than \texttt{ExprF}—while retaining the expressive folding power
granted by paramterized data types. This is cool as hell. This technique is used in other libraries,
such as José Pedro Magalhães's \href{https://hackage.haskell.org/package/regular/docs/Generics-Regular-Base.html#t:PF}{\texttt{regular}}.

The implementation of \texttt{Recursive} for \texttt{[a]} follows. We convert the empty list \texttt{[]} into \texttt{Nil}, and replace \texttt{:} with the \texttt{Cons} constructor.

\begin{code}
instance Recursive [a] where
  project (x:xs) = Cons x xs
  project [] = Nil
\end{code}

Another valuable instance is one for \texttt{Natural}—as we discussed in the previous installment, we can
fold over the natural numbers, bottoming out when we hit zero. We built our own \texttt{Nat} data type,
observing that it was equivalent in definition to \texttt{Maybe}—\texttt{recursion-schemes} just uses
\texttt{Maybe} for its \texttt{Recursive} and \texttt{Base} instance for \texttt{Natural}.

\begin{code}
type instance Base Natural = Maybe

instance Recursive Natural where
  project 0 = Nothing
  project n = Just (n - 1)
\end{code}

\subsubsection{Yet More Concision}\label{yet-more-concision}

As we've mentioned before, given a data type \texttt{t}, the steps for constructing its \texttt{Base} instance
are straightforward: add a new type variable to the definition, and for each data constructor, create a new
constructor with all recursive occurrences of \texttt{t} replaced by the new type variable.

Thanks to the magic of Template Haskell, \texttt{recursion-schemes} can generate this code for us:

\begin{verbatim}
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
\end{verbatim}

The \texttt{makeBaseFunctor} call generates code equivalent to the following:

\begin{verbatim}
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
  project (Index a b) = IndexF (project a) (project b)
  project (Call a b)  = CallF (project a) (fmap project b)
  -- and so on and so forth
\end{verbatim}

This is clearly the result of applying the aforementioned algorithm to our \texttt{Expr} type. To avoid
name collisions, constructor names are suffixed with an `F'. (Infix operators are suffixed with a `\$').

The inclusion of these Template Haskell splices means that you, the programmer, can pick up the
\texttt{recursion-schemes} library with a minimum of fuss and boilerplate. This is, in my experience writing
Haskell in production, truly invaluable when dealing with nested data types: you can fold beautifully
without setting up dozens of lines of boilerplate.

\subsubsection{Reversing the Arrows, Again}\label{reversing-the-arrows}

In part two of this series, we generated an unfold by `reversing the arrows' in \texttt{cata}. As you
might be able to infer from the title of this section, we can do the same for \texttt{Recursive}, which
yields us a \texttt{Corecursive} typeclass for unfolds:

\begin{code}
class Functor (Base t) => Corecursive t where
  embed :: Base t t -> t
  ana :: (a -> Base t a) -> a -> t
\end{code}

We've already reversed the arrows and generated \texttt{ana} from \texttt{cata}. The only thing left to
do is reverse the arrows in \texttt{project}, yielding \texttt{embed}—rather than going from a \texttt{t} to
a \texttt{Base} functor, as \texttt{project} does, we go from a \texttt{Base} functor to a \texttt{t}.

As with \texttt{cata}, \texttt{ana} is defined in terms of \texttt{fmap} and \texttt{embed}:

\begin{verbatim}
class Functor (Base t) => Corecursive t where
  embed :: Base t t -> t
  ana
    :: (a -> Base t a) -- ^ a (Base t)-coalgebra
    -> a               -- ^ seed
    -> t               -- ^ resulting fixed point
  ana g = a where a = embed . fmap a . g
\end{verbatim}

Instances for \texttt{embed} are similarly straightforward:

\begin{verbatim}
instance Corecursive [a] where
  embed (Cons x xs) = x:xs
  embed Nil = []
\end{verbatim}


In practice, you won't need to write your own \texttt{Corecursive} instances, as \texttt{makeBaseFunctor}
creates both \texttt{Recursive} and \texttt{Corecursive} instances.

\subsubsection{One More Aside}\label{one-more-aside}

Particularly alert readers will notice that the definition for \texttt{cata} provided by Kmett in
\texttt{recursion-schemes} is slightly different from ours. Our definition used partial application
of \texttt{cata} in its definition—\texttt{cata f}, being partially applied to \texttt{f}, can be passed
to \texttt{fmap}:

\begin{verbatim}
cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f
\end{verbatim}

By contrast, Kmett's \texttt{cata} uses a where-clause to capture \texttt{cata f} with a specific name,
\texttt{c}.

\begin{verbatim}
cata :: (Base t a -> a) -> t -> a
cata f = c where c = f . fmap c . project
\end{verbatim}

Both formulations of \texttt{cata} are defined point-free, but Kmett's struck me as somewhat unusual—
the name \texttt{c} appears unnecessary, given that you can just pass \texttt{cata f} to \texttt{fmap}.
It took several years before I inferred the reason behind this—GHC generates more efficient code if you
avoid partial applications. Partially-applied functions must carry their arguments along with them,
forcing their evaluation process to dredge up the applied arguments and call them when invoking the function.
whereas bare functions are much simpler to invoke. (For more of the gory details, you can consult the GHC wiki page on the representation of \href{https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects}{heap objects}).

Bearing this implementation detail in mind, this formulation of \texttt{cata} is quite elegant: by naming
\texttt{cata f}, we can reference it not as a partially applied function, but as a regular function.
Passing that function into \texttt{fmap} generates more efficient code—usually this kind of microoptimization
doesn't matter much, but given that \texttt{cata} is invoked on every iteration of a fold, the savings add up.

\subsubsection{That's All}\label{thats-all}

I owe thanks to Edward Kmett, whose \texttt{recursion-schemes} library is magnificent and inspiring, and to
Austin Seipp, who checked my statements about GHC code generation for accuracy.

I'm hoping to be done with the next proper installment of this series within the next couple of weeks. (This
one covers hylomorphisms and chronomorphisms.) Until then, thank you for reading.

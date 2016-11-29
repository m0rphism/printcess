module Printcess.PrettyPrinting (
  -- * Overview
  -- $overview

  -- * Example
  -- $example

  -- * Rendering
  pretty,
  prettyPrint,

  -- * Config
  Config,
  cMaxLineWidth, cIndentChar, cIndentDepth, cIndentAfterBreaks,
  cInitIndent, cInitPrecedence,
  defConfig,

  -- * Type Class
  Pretty(..),

  -- * Monad
  PrettyM,

  -- * Sequencing
  (+>), (~>), (\>),

  -- * Indentation
  indentedByChars, indentedBy, indented,
  block, block',

  -- * Associativity & Fixity
  assocL, assocR, assocN,
  left, right, inner, AssocAnn(..),

  -- * Folding Lists of @Pretty@ Things
  sepBy, sepByL, sepByR,
  ppList, ppSExp,

  -- * Folding Maps of @Pretty@ Things
  ppListMap, ppMap,

  -- * Other combinators
  ppParen,
  ppBar,

  -- * Constants
  nl, sp,

  -- * Lifted Type Classes
  Pretty1(..), Pretty2(..),

  -- * Reexports
  State, (.=),
  ) where

import Control.Monad.State.Strict
import Control.Lens

import Printcess.Config
import Printcess.Core
import Printcess.Combinators

{- $overview
    The main features of the @printcess@ pretty printing library are

    *   /Indentation/. Printing-actions are relative to the indentation level
        of their context. Special actions can be used to control the indentation
        level. Indentation is automatically inserted after newlines.

    *   /Automatic parenthesizing of mixfix operators/.
        Special printing-actions can be used to specify the associativity
        and fixity of operators and to mark the positions of their arguments.
        This makes it easy to print for example @"λx. λy. x y (x y)"@
        instead of @"(λx. (λy. ((x y) (x y))))"@.

    *   /Automatic line breaks after exceeding a maximum line width/.
        A maximum line width can be specified, after which lines are
        automatically broken. If the break point is inside a word,
        it is moved to the left until a white space character is reached.
        This avoids splitting identifiers into two.
-}

{- $example
    In this section, a small example is presented, which pretty prints a
    lambda calculus expression.

    First we define an abstract syntax tree for lambda calculus expressions.

    > data Expr
    >   = EVar String
    >   | EAbs String Expr
    >   | EApp Expr Expr

    Then we make @Expr@ an instance of the 'Pretty' type class, which
    declares one method 'pp'. This method takes an @Expr@ and returns a
    'PrettyM' @()@ action, which describes how to 'pretty' print the @Expr@.

    > instance Pretty Expr where
    >   pp (EVar x)     = pp x
    >   pp (EApp e1 e2) = assocL 9 $ L e1 ~> R e2
    >   pp (EAbs x e)   = assocR 0 $ "λ" +> I x +> "." ~> R e

    We print

    *   a variable @EVar x@ by printing the identifier 'String' @x@.

    *   a function application @EApp e1 e2@ as a left-associative operator of
        fixity 9 ('assocL' @9@), where e1 is the left argument ('L') and @e2@ is
        the right argument ('R'). The ('~>') combinator separates its first
        argument with a space from its second argument.

    *   a function abstraction @EAbs x e@ as a right-associative operator of
        fixity 0 ('assocR' @0@), where @x@ is an inner
        argument ('I') and @e@ is the right argument ('R').
        The ('+>') combinator behaves as ('~>'), but without inserting a space.

    Then we define a simple test expression @e1@ representing @λx. λy. x y (x y)@

    > e1 :: Expr
    > e1 = EAbs "x" $ EAbs "y" $ EApp (EApp (EVar "x") (EVar "y"))
    >                                 (EApp (EVar "x") (EVar "y"))

    and pretty print it to 'String' using the 'pretty' function

    > s1, s2 :: String
    > s1 = pretty defConfig                  e1    -- evaluates to "λx. λy. x y (x y)"
    > s2 = pretty (cMaxLineWidth .= Just 12) e1    -- evaluates to "λx. λy. x y
    >                                              --                   (x y)"
-}

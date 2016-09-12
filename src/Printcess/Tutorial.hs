{-# LANGUAGE LambdaCase, UnicodeSyntax #-}

module Printcess.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Basic Printing
    -- $basicprinting
    -- * Keeping Indentation Levels
    -- * Automatic Line Breaks
    -- * Operator Associativity & Fixity

    -- * Module
    Expr(..),
    expr0, expr1, expr2,
    main0,
  ) where

import Printcess.PrettyPrinting

{- $introduction
    The @printcess@ library provides combinators for pretty printing.
    It supports

    * indentation aware printing;
    * automatic line breaks with temporarily increased indentation on text-width exceedance;
    * fixity and associativity sensitive printing of operators.

    Having a well readable representation of the intermediate and target languages
    can be very valuable for debugging.

    The following examples will showcase the different features of the library.
-}

{- $basicprinting
    In this section we will use @printcess@ to describe a simple pretty printer for an
    abstract syntax tree (AST) of the lambda calculus.

    The following data type describes an AST for the lambda calculus using
    @String@s as identifiers.

    > data Expr              -- An expression can be either
    >   = EVar String        -- * a variable expression
    >   | EAbs String Expr   -- * a lambda abstraction binding
    >   | EApp Expr Expr

-}

type Ident = String

{-
    In this example, we
    Consider a data type for an abstract syntax tree of the lambda calculus:

    > data Expr
    >   = EVar String
    >   | EAbs String Expr
    >   | EApp Expr Expr

    Function composition can be expressed as followed:

    > --            λf.        λg.        λx.              f              (g          x)
    > composeExpr = EAbs "f" $ EAbs "g" $ EAbs "x" $ EApp (EVar "f" (EApp (EVar "g") (EVar "x")))


-}

data Expr
  = EVar Ident
  | EAbs Ident Expr
  | EApp Expr Expr

instance Pretty Expr where
  pp = \case
    EVar i     → pp i
    EAbs i e   → assocR 0 $ "λ" +> i +> ". " +> R e
    EApp e1 e2 → assocL 9 $ L e1 +> " " +> R e2

expr0, expr1, expr2 :: Expr
expr0 = EApp (EVar "x") (EVar "y")
expr1 = EAbs "x" $ EAbs "y" $ EApp expr0 expr0
expr2 = foldl EApp expr0 (replicate 20 expr0)

main0 :: IO ()
main0 = do
  prettyPrint def $ "Printcess Pretty Printing Example"
  prettyPrint def expr1
  prettyPrint (def { configMaxLineWidth = 20 }) expr2
  -- prettyPrint' (configMaxLineWidth .= 20) expr2

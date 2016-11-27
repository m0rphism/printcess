{-# LANGUAGE LambdaCase, UnicodeSyntax #-}

module Printcess.Tutorial (
    -- * Introduction
    -- $introduction
    -- expr0,

    -- * Basic Printing
    -- $basicprinting
    -- expr1,

    -- * Making an AST printable
    -- $basicast
    -- expr2,

    -- * Keeping Indentation Levels

    -- * Automatic Line Breaks

    -- * Operator Associativity & Fixity

    -- * Module
    Expr(..),
    -- expr0, expr1, expr2,
    main0,
  ) where

import Printcess.PrettyPrinting

{- $introduction
    The @printcess@ library provides combinators for pretty printing.
    It supports

    * indentation aware printing;

      TODO: move to indentation chapter

      > pretty def $ "do\n" +> block [ "putStrLn hello"
      >                              , "putStrLn world" ]
      prints

      > do
      >   putStrLn hello
      >   putStrLn world

    * indented line breaks when exceeding a maximum line length;
    * fixity and associativity sensitive printing of operators.

    Having a well readable representation of the intermediate and target languages
    can be very valuable for debugging.

    The following examples will showcase the different features of the library.
-}

{- $basicprinting
    In this section, it is explained how simple @String@s can be printed using different configurations.

    The library organizes pretty printable things in the @Pretty@ type class.
    Pretty printable things can be printed to @String@ using the @pretty@ function.

    > pretty :: Pretty a => Config → a → String

    The config parameter describes how the @a@ should be printed.
    @Config@ has a @Data.Default@ instance and lenses for the configuration options.

    The library provides @Pretty@ instances for a few base types including @String@ and @Int@.
    The following code uses them to print a @String@ and an @Int@ using the @pretty@ function:

    > pretty def "foo"    -- evaluates to "foo"
    > pretty def 2        -- evaluates to "2"

    @Pretty@ printable things can be printed in sequence using the @(+>)@ operator.

    > (+>) : (Pretty a, Pretty b) => a → b → PrettyM ()

    This is illustrated in the next example:

    > pretty def $ "foo" +> 2  -- evaluates to "foo2"

    -- @configMaxLineWidth@' :: Lens' Config Int'


-}

{- $basicast
    In this section we will use @printcess@ to describe a simple pretty printer for an
    abstract syntax tree (AST) of the lambda calculus.

    The following data type describes an AST for the lambda calculus using
    @String@s as identifiers.

    > data Expr              -- An expression can be either
    >   = EVar String        -- * a variable
    >   | EAbs String Expr   -- * a function abstraction
    >   | EApp Expr Expr     -- * a function application

    Making an instance for @Expr@ in the @Pretty@ type class, makes an @Expr@
    pretty printable.

    > instance Pretty Expr where
    >   pp = \case
    >     EVar i     → pp i
    >     EAbs i e   → "λ" +> i +> ". " +> e
    >     EApp e1 e2 → e1 +> " " +> e2

    Variables should be printed

    > instance Pretty Expr where
    >   pp = \case
    >     EVar i     → pp i
    >     EAbs i e   → assocR 0 $ "λ" +> i +> ". " +> R e
    >     EApp e1 e2 → assocL 9 $ L e1 +> " " +> R e2
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
  prettyPrint (configMaxLineWidth .= 20) expr2

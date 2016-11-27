{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase, UnicodeSyntax, MultiWayIf, KindSignatures #-}

module Printcess.PrettyPrinting (
  -- * Overview
  -- $overview

  -- * Rendering
  pretty,
  prettyPrint,

  -- * Config
  Config(..),
  configMaxLineWidth, configInitPrecedence, configInitIndent,
  configIndentChar, configIndentDepth, configIndentAfterBreaks,
  def,
  State, (.=),

  -- * Type Classes
  Pretty(..), Pretty1(..), Pretty2(..),

  -- * Monad
  PrettyM,

  -- * Basic Combinators
  (+>),
  (++>),

  -- * Indentation
  indent, unindent, indented,

  -- * Associativity & Fixity
  assocL, assocR, assocN,
  left, right, inner, AssocAnn(..),

  -- * Folding Lists of Printable Things
  sepBy, interleaveL, interleaveR,
  block, block',
  ppList, ppSExp,

  -- * Folding Maps of Printable Things
  ppListMap, ppMap,

  -- * Misc
  ifPrint,
  ppParen,
  ppBar,

  -- * Constants
  nl, sp,

  test,
  ) where

import Control.Applicative
import Control.Monad.State.Lazy
import Control.Lens
import Data.Foldable
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

{- $overview
    The main features of the @printcess@ pretty printing library are

    *   Indentation.

        > pretty def $ "do" +> block [ "putStrLn hello"
        >                            , "putStrLn world" ]
        prints

        > do
        >   putStrLn hello
        >   putStrLn world

    *   Parenthesis-Elision according to fixity and associativity of operators.

        > let eAbs x  e  = assocR 0 $ "λ" +> x +> "." ++> R e
        >     eApp e1 e2 = assocL 9 $ L e1 ++> R e2
        > in pretty def $ eAbs "x" $ eAbs "y" $ eApp (eApp "x" "y") (eApp "x" "y")
        prints

        > λx. λy. x y (x y)

    *   Automatic line breaks after current line exceeds a maximum text width.

        > pretty (def & configMaxLineWidth .~ 10) "foo bar baz boo r"
        prints

        > foo bar
        >     baz
        >     boo r

-}

-- Config ----------------------------------------------------------------------

-- | A @Config@ describes how something can be @Pretty@ printed, and hence is
-- consumed mainly by the @pretty@ function.
--
-- A @Config@ can be @Default@ constructed via the @def@ method and manipulated
-- via lenses for its fields. This is illustrated in the following example,
-- creating a @Config@ with maximum line width of @6@ and an initial indentation
-- level of @1@:
--
-- > import Control.Lens ((&), (.~))
-- > import Data.Default (def)
-- >
-- > foo :: String
-- > foo = pretty config "foo bar baz"
-- >   where config = def & configMaxLineWidth .~ 6
-- >                      & configInitIndent   .~ 1
--
-- To preserve the maximum line width, the @String@ @"foo bar baz"@ is split
-- into 3 lines by replacing spaces with newlines. Because of the initial
-- indentation, each line gets indented by one level, which here corresponds to
-- 2 spaces:
--
-- >   foo
-- >   bar
-- >   baz
data Config = Config
  -- { configSpacesPerIndent :: Int
  {
  -- | When a line gets longer, it is broken after the latest space,
  --   that still allows the line to remain below this maximum.
  --
  --  Default: @80@
    _configMaxLineWidth :: Int
  -- | Precendence level to start pretty printing with.
  --
  --  Default: @(-1)@
  , _configInitPrecedence :: Int
  -- | Indentation level to start pretty printing with.
  --
  --   Default: @0@
  , _configInitIndent :: Int
  -- | The character to indent after line breaks with. Usually @' '@ for spaces
  --   or @'\t'@ for tabs.
  --
  --   Default: @' '@
  , _configIndentChar :: Char
  -- | How many characters to indent after line breaks with.
  --
  --   Default: @2@
  , _configIndentDepth :: Int
  -- | How much to increase indentation when a line break occurs because @configMaxLineWidth@ was exceeded.
  --
  --   Assuming the line to print has to be broken multiple times, the
  --   indentation of all resulting lines, except the first one, is increased by this amount.
  --   For example @"foo bar baz boo"@ may be printed to
  --
  --   > foo bar
  --   >     baz
  --   >     boo
  --
  --   using default configuration with @configMaxLineWidth .~ 8@.
  --
  --   Default: @2@
  , _configIndentAfterBreaks :: Int
  }

def :: State Config ()
def = pure ()

defConfig :: Config
defConfig = Config
    { _configMaxLineWidth    = 80
    , _configInitPrecedence  = -1
    , _configInitIndent      = 0
    , _configIndentChar      = ' '
    , _configIndentDepth     = 2
    , _configIndentAfterBreaks = 2
    }

makeLenses ''Config

data Assoc = AssocN | AssocL | AssocR
  deriving (Eq, Ord, Read, Show)

data PrettySt = PrettySt
  { _indentation       :: Int
  , _precedence        :: Int
  , _assoc             :: Assoc
  , _maxLineWidth      :: Int
  , _text              :: NE.NonEmpty String
  , _indentChar        :: Char
  , _indentDepth       :: Int
  , _indentAfterBreaks :: Int
  }

stFromConfig :: Config → PrettySt
stFromConfig c = PrettySt
  { _indentation       = _configInitIndent c
  , _precedence        = _configInitPrecedence c
  , _assoc             = AssocN
  , _maxLineWidth      = _configMaxLineWidth c
  , _text              = "" :| []
  , _indentChar        = _configIndentChar c
  , _indentDepth       = _configIndentDepth c
  , _indentAfterBreaks = _configIndentAfterBreaks c
  }

makeLenses ''PrettySt

-- Pretty Printing -------------------------------------------------------------

-- | Render a 'Pretty' printable @a@ to 'String' using a 'Config', that
--   specifies how the @a@ should be rendered.
--
--   The following example uses the default configuration to render @1@:
--
--   > pretty def (1 :: Int)  -- evaluates to "1"
pretty :: Pretty a => State Config () → a → String
pretty c
  = concat
  . (`sepByL` "\n")
  . reverse
  . NE.toList
  . view text
  . (`execState` stFromConfig (execState c defConfig))
  . runPrettyM
  . pp
  . (addIndent +>) -- Add indentation to the first line.
                   -- The other lines are indented when they are reached.

-- | Render a 'Pretty' printable @a@ to @stdout@ using a 'Config', that
--   specifies how the @a@ should be rendered.
--
--   Convenience function, defined as:
--
--   > prettyPrint c = liftIO . putStrLn . pretty c
prettyPrint :: (MonadIO m, Pretty a) => State Config () → a → m ()
prettyPrint c = liftIO . putStrLn . pretty c

-- Type Classes ----------------------------------------------------------------

-- | The 'Pretty' type class describes pretty printable types.
--
-- Types which instanciate this class can be combined with each other, e.g.
-- printed in sequence with @(+>)@, and rendered to @String@ using the @pretty@
-- function.
--
-- The library provides instances for some base types, including @String@ and @Int@,
-- which are used in the following example to print @"foo"@ in sequence with @1@:
--
-- > pretty def ("foo" +> 1)    -- evaluates to "foo1"
--
-- Consider a simple data type for integer arithmetic
--
-- > data Expr
-- >   = EInt Int
-- >   | EAdd Expr Expr
-- >   deriving (Eq, Ord, Read, Show)
--
-- To pretty print the expression
--
-- > expr = EAdd (EInt 1) (EAdd (EInt 2) (EInt 3))
--
-- as @"(1+(2+3))"@, we can make @Expr@ an instance of the @Pretty@ class
--
-- > instance Pretty Expr where
-- >   pp = \case
-- >     EInt i     → pp i  -- Use the Pretty instance for Int
-- >     EAdd e1 e2 → "(" +> e1 ++> "+" ++> e2 +> ")"
--
-- and then render it to @String@ with @pretty def expr@.
class Pretty a where
  -- | Pretty print an @a@ as a 'PrettyM' action.
  pp :: a → PrettyM ()

head1L :: Lens' (NE.NonEmpty a) a
head1L = lens NE.head (\(_ :| xs) x → x :| xs)

tail1L :: Lens' (NE.NonEmpty a) [a]
tail1L = lens NE.tail (\(x :| _) xs → x :| xs)

charsBeforeWord :: Int -> (Char -> Bool) -> String -> Int
charsBeforeWord nWords0 cIndent s0 =
  go s0 nWords0
 where
  go s n =
    length sIndent + if n == 0 then 0 else length sWord + go sAfterWord (n-1)
   where
    (sIndent, sBeforeWord) = break (not . cIndent) s
    (sWord, sAfterWord) = break cIndent sBeforeWord

charsBeforeWordM :: Int -> PrettyM Int
charsBeforeWordM n0 = do
  cIndent ← use indentChar
  curText ← use $ text . head1L
  pure $ charsBeforeWord n0 (`elem` [' ', '\t', cIndent]) curText

-- Isomorphic lines and unlines implementations
lines' :: String → [String]
lines' = go "" where
  go s = \case
    ""               → [s]
    c:cs | c == '\n' → s : go "" cs
         | otherwise → go (s++[c]) cs

unlines' :: [String] → String
unlines' = \case
  []   → ""
  [x]  → x
  x:xs → x ++ '\n' : unlines' xs

isWS, isNoWS :: Char → Bool
isWS   = (`elem` [' ', '\t'])
isNoWS = not . isWS

instance Pretty String where
  pp = go . lines' where
    go :: [String] -> PrettyM ()
    go []     = pure ()
    go [s]    = ppLine True s
    go (s:ss) = do ppLine True s; nl; go ss

    ppLine :: Bool -> String -> PrettyM ()
    ppLine first s = do
      text . head1L %= (++s)
      curLine ← use $ text . head1L
      -- We have to allow at least indentation + 1 word, otherwise indenting after
      -- line break due to line continuation can cause infinite loop
      maxLineLength ← max <$> charsBeforeWordM 1 <*> use maxLineWidth
      when (length curLine > maxLineLength) $ do
        let (curLine', lineRest) = splitAt maxLineLength curLine -- length s1 == maxLineLength
        let (wordRest, curLine'')
              | isNoWS (head lineRest)
                = both %~ reverse $ break (==' ') $ reverse curLine'
              | otherwise
                = ("", curLine')
        text . head1L .= curLine''
        -- Increase indentation once after the first forced line break, this results into:
        --   this line was too long
        --       still the first line
        --       it won't stop
        i ← use indentAfterBreaks
        let f | first     = foldl (.) id (replicate i indented)
              | otherwise = id
        f $ do nl; ppLine False $ dropWhile isWS $ wordRest ++ lineRest

instance Pretty Char   where pp = pp . (:[])
instance Pretty Int    where pp = pp . show
instance Pretty Float  where pp = pp . show
instance Pretty Double where pp = pp . show
instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pp = foldl pp' (pp "") . M.toList where
    pp' s (k, v) = s +> k ++> "=>" ++> indented v +> nl

-- | The 'Pretty1' type class lifts pretty printing to unary type constructors.
--   It can be used in special cases to abstract over type constructors which
--   are pretty printable for any pretty printable type argument.
class Pretty1 f where
  pp1 :: Pretty a => f a → PrettyM ()
  default pp1 :: Pretty (f a) => f a -> PrettyM ()
  pp1 = pp

-- | The 'Pretty2' type class lifts pretty printing to binary type constructors.
--   It can be used in special cases to abstract over type constructors which
--   are pretty printable for any pretty printable type argument.
class Pretty2 (f :: * → * → *) where
  pp2 :: (Pretty a, Pretty b) => f a b → PrettyM ()
  default pp2 :: Pretty (f a b) => f a b -> PrettyM ()
  pp2 = pp

-- Pretty Monad ----------------------------------------------------------------

-- | The 'PrettyM' monad is run in the pretty printing process, e.g. in
--   'pretty' or 'prettyPrint'.

--   A monoid could have been used instead, but with a monad the @do@ notation
--   can be used to print in sequence with semicolons.
newtype PrettyM a = PrettyM { runPrettyM :: State PrettySt a }
  deriving (Functor, Applicative, Monad, MonadState PrettySt)

instance Pretty (PrettyM ()) where pp = (>> return ())
-- instance a ~ () => IsString (PrettyM ()) where
--   fromString = pp
-- instance Pretty (PrettyM a) where pp = (>> return ())
instance a ~ () => Monoid (PrettyM a) where
  mempty = pure mempty
  mappend = liftA2 mappend


-- Basic Combinators -----------------------------------------------------------

-- | Print two 'Pretty' printable things in sequence.
--
--   Example:
--
--   > pretty def $ "x" +> 1  -- ↪ "x1"
--
--   Convenience function, defined as
--
--   > a +> b = pp a >> pp b
(+>) :: (Pretty a, Pretty b) => a → b → PrettyM ()
a +> b = pp a >> pp b

-- | Print two 'Pretty' printable things in sequence, separated by a space.
--
--   Example:
--
--   > pretty def $ "x" ++> 1  -- ↪ "x 1"
--
--   Convenience function, defined as
--
--   > a ++> b = a +> " " +> b
(++>) :: (Pretty a, Pretty b) => a → b → PrettyM ()
a ++> b = a +> sp +> b

-- Indentation -----------------------------------------------------------------

-- | Increment indentation level.
indent :: PrettyM ()
indent = (indentation +=) =<< use indentDepth

-- | Decrement indentation level.
unindent :: PrettyM ()
unindent = (indentation -=) =<< use indentDepth

-- | Print an @a@ using an incremented indentation after newlines.
--
--   Example:
--
--   > pretty def $ "while (true) {" +>
--   >              indented (nl +> "f();" +> nl +> "g();") +>
--   >              nl +> "}"
--   > ↪ pretty def $ "while (true) {" +>
--   >              block ["f();", "g();"] +>
--   >              nl +> "}"
--   > ↪ "while (true) {
--   >      f();
--   >      g();
--   >    }"
--
--   Convenience function, defined as
--
--   > indented a = indent +> a +> unindent
indented :: Pretty a => a → PrettyM ()
indented a = do indent; pp a; unindent

addIndent :: PrettyM ()
addIndent = do
  i <- use indentation
  c <- use indentChar
  text . head1L %= (++ replicate i c)

indentToCurPos :: PrettyM ()
indentToCurPos = do
  curLine ← use $ text . head1L
  indentation .= length curLine

indentedToCurPos :: PrettyM a → PrettyM a
indentedToCurPos ma = do
  i ← use indentation
  indentToCurPos
  a ← ma
  indentation .= i
  pure a

-- Associativity & Fixity ------------------------------------------------------

withPrecedence :: (Assoc, Int) → PrettyM () → PrettyM ()
withPrecedence (a, p) ma = do
  p' ← use precedence
  a' ← use assoc
  precedence .= p
  assoc .= a
  if | p' == p && a' == a && a /= AssocN → ma
     | p' < p    → ma
     | otherwise → do pp "("; ma; pp ")"
  precedence .= p'
  assoc .= a'

-- | Print an @a@ as a left-associative operator of a certain fixity.
assocL :: Pretty a => Int → a → PrettyM ()
assocL i = withPrecedence (AssocL, i) . pp

-- | Print an @a@ as a right-associative operator of a certain fixity.
assocR :: Pretty a => Int → a → PrettyM ()
assocR i = withPrecedence (AssocR, i) . pp

-- | Print an @a@ as a non-associative operator of a certain fixity.
assocN :: Pretty a => Int → a → PrettyM ()
assocN i = withPrecedence (AssocN, i) . pp

-- | Print an @a@ as a left, right, or inner argument of a mixfix operator.
data AssocAnn a = L a | R a | I a
  deriving (Eq, Ord, Read, Show)

instance Pretty1 AssocAnn
instance Pretty a => Pretty (AssocAnn a) where
  pp = \case
    L a → left a
    R a → right a
    I a → inner a

-- | Print an @a@ as the left argument of a mixfix operator.
left :: Pretty a => a → PrettyM ()
left = assocDir AssocL

-- | Print an @a@ as the right argument of a mixfix operator.
right :: Pretty a => a → PrettyM ()
right = assocDir AssocR

-- | Print an @a@ as an inner argument of a mixfix operator.
inner :: Pretty a => a → PrettyM ()
inner ma = do
  p' ← use precedence
  a' ← use assoc
  precedence .= (-1)
  assoc .= AssocN
  pp ma
  precedence .= p'
  assoc .= a'

assocDir :: Pretty a => Assoc → a → PrettyM ()
assocDir a ma = do
  a' ← use assoc
  if | a' == a → pp ma
     | otherwise → do
       assoc .= AssocN
       pp ma
       assoc .= a'

-- Composite Combinators -------------------------------------------------------

-- | Put an @a@ between each element of a @[b]@ and then print them in sequence.
--
--   Examples:
--
--   > pretty def $ []         `sepBy` ","  -- ↪ ""
--   > pretty def $ ["x"]      `sepBy` ","  -- ↪ "x"
--   > pretty def $ ["x", "y"] `sepBy` ","  -- ↪ "x,y"
sepBy :: (Pretty a, Pretty b) => [b] → a → PrettyM ()
sepBy as s = sepByA_ (map pp as) (pp s)

sepByA_ :: Applicative f => [f a] → f a → f ()
sepByA_ as s = () <$ sepByA as s

sepByA :: Applicative f => [f a] → f a → f [a]
sepByA []  _    = pure []
sepByA [a] _    = (:[]) <$> a
sepByA (a:as) s = (\x y z → x:y:z) <$> a <*> s <*> sepByA as s

sepByL :: [[a]] → [a] → [[a]]
sepByL []  _    = []
sepByL [s] _    = [s]
sepByL (s:ss) s' = s : s' : sepByL ss s'

-- | Put an @a@ before each element of a @[b]@ and then print them in sequence.
--
--   Examples:
--
--   > pretty def $ interleaveL "," []          -- ↪ ""
--   > pretty def $ interleaveL "," ["x"]       -- ↪ ",x"
--   > pretty def $ interleaveL "," ["x", "y"]  -- ↪ ",x,y"
interleaveL :: (Pretty a, Pretty b) => a → [b] → PrettyM ()
interleaveL a bs = fold $ interleaveL' (pp a) (pp <$> bs)

-- | Put an @a@ after each element of a @[b]@ and then print them in sequence.
--
--   Examples:
--
--   > pretty def $ interleaveR "," []          -- ↪ ""
--   > pretty def $ interleaveR "," ["x"]       -- ↪ "x,"
--   > pretty def $ interleaveR "," ["x", "y"]  -- ↪ "x,y,"
interleaveR :: (Pretty a, Pretty b) => a → [b] → PrettyM ()
interleaveR a bs = fold $ interleaveR' (pp a) (pp <$> bs)

interleaveL' :: a → [a] → [a]
interleaveL' s = foldl (\xs x → xs ++ [s,x]) []
interleaveR' :: a → [a] → [a]
interleaveR' s = foldl (\xs x → xs ++ [x,s]) []

-- splitAtDelim :: String → Char → [String]
-- splitAtDelim s' c = go [] s' where
--   go s [] = [reverse s]
--   go s (x:xs) | x == c    = reverse s : go "" xs
--               | otherwise = go (x:s) xs

-- | Print a @[a]@ as a block, meaning that the indentation level is
-- increased, and each @a@ is printed on a single line.
--
-- Example:
--
-- > pretty def $ "do" ++> block ["putStrLn hello", "putStrLn world"]
-- > -- ↪ "do
-- > --      putStrLn hello
-- > --      putStrLn world"
block :: Pretty a => [a] → PrettyM ()
block  xs = indented $ nl +> (xs `sepBy` nl)

-- | Same as @block@, but starts the block on the current line.
--
-- Example:
--
-- > pretty def $ "do" ++> block' ["putStrLn hello", "putStrLn world"]
-- > -- ↪ "do putStrLn hello
-- > --       putStrLn world"
block' :: Pretty a => [a] → PrettyM ()
block' xs = indentedToCurPos $ xs `sepBy` nl

-- | If @True@ print an @a@; if @False@ print @""@.
ifPrint :: Pretty a => Bool → a → PrettyM ()
ifPrint True  a = pp a
ifPrint False _ = pp ""

-- | Print a @[a]@ similar to its @Show@ instance.
--
--   Example:
--
--   > pretty def $ ppList [ "x", "y" ]  -- ↪ "[ x, y ]"
--
--   Convenience function, defined as:
--
--   > ppList ps = "[" ++> (ps `sepBy` ", ") ++> "]"
ppList :: Pretty a => [a] → PrettyM ()
ppList ps = "[" ++> (ps `sepBy` ", ") ++> "]"

-- | Print a list map @[(k,v)]@ as @ppList@, but render @(k,v)@ pairs as @"k → v"@.
--
--   Example:
--
--   > pretty def $ ppListMap [ ("k1", "v1"), ("k2", "v2") ]  -- ↪ "[ k1 → v1, k2 → v2 ]"
--
--   Convenience function, defined as:
--
--   > ppListMap = block . map (\(a,b) → a ++> "→" ++> b)
ppListMap :: (Pretty a, Pretty b) => [(a, b)] → PrettyM ()
ppListMap = block . map (\(a,b) → a ++> "→" ++> b)

-- | Print a @Data.Map@ in the same way as @ppListMap@.
ppMap :: (Pretty a, Pretty b) => M.Map a b → PrettyM ()
ppMap = ppListMap . M.assocs

-- | Print an @a@ in parentheses.
--
--   Example:
--
--   > pretty def $ ppParen "foo"  -- ↪ "(foo)"
--
--   Convenience function, defined as:
--
--   > ppParen x = "(" +> x +> ")"
ppParen ∷ Pretty a ⇒ a → PrettyM ()
ppParen x = "(" +> x +> ")"

-- | Print a @[a]@ as an LISP like SExpr, that is the elements separated by
--   spaces and enclosed in parentheses.
--
--   Example:
--
--   > pretty def $ ppSExp ["+", "2", "3"]  -- ↪ "(+ 2 3)"
--
--   Convenience function, defined as:
--
--   > ppSExp = ppParen . (`sepBy` sp)
ppSExp ∷ Pretty b ⇒ [b] → PrettyM ()
ppSExp = ppParen . (`sepBy` sp)

-- | Print a horizontal bar consisting of a @Char@ as long as the max line width.
--   The horizontal bar has a title @String@ printed at column 6.
--
--   Example:
--
--   > pretty def $ ppBar '-' "Foo"
--   > -- ↪ "----- Foo -------------------------------…"
ppBar ∷ Pretty a => Char → a → PrettyM ()
ppBar c s = do
  w ← use maxLineWidth
  replicate 5 c ++> s ++> replicate (w - (7 + length (pretty (pure ()) s))) c +> "\n"


-- | Print a newline (line break).
nl :: PrettyM ()
nl = do
  text %= ("" NE.<|)
  addIndent

-- | Print a space.
sp :: PrettyM ()
sp = pp " "

test = do
  putStrLn $
    pretty def $ "do" ++> block [ "putStrLn hello"
                                , "putStrLn world" ]
  putStrLn $
    pretty def $ "do" ++> block' [ "putStrLn hello"
                                 , "putStrLn world" ]

  let eAbs x e = assocR 0 $ "λ" +> x +> "." ++> R e
      eApp e1 e2 = assocL 6 $ L e1 ++> R e2
  putStrLn $
    pretty def $ eAbs "x" $ eAbs "y" $ eApp (eApp "x" "y") (eApp "x" "y")

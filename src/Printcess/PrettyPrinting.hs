{-
  TODO:
    - support for different amounts of tabs or spaces per indent
-}
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
  -- * Rendering
  pretty,
  prettyPrint,
  -- * Config
  Config(..),
  configMaxLineWidth, configInitPrecedence, configInitIndent,
  configIndentChar, configIndentDepth, configIndentAfterBreaks, configBlockStyle,
  Data.Default.def,
  BlockStyle(..),
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
  ) where

import Control.Applicative
import Control.Monad.State.Lazy
import Control.Lens
import Data.Foldable
import Data.Default
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

data PrettySt = PrettySt
  { _indentation  :: Int
  , _precedence   :: Int
  , _assoc        :: Assoc
  , _maxLineWidth :: Int
  , _text         :: NE.NonEmpty String
  }

data Assoc = AssocN | AssocL | AssocR
  deriving (Eq, Ord, Read, Show)

makeLenses ''PrettySt

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
  -- | How to display items of a block.
  --
  --   @BSImplicit@ displays blocks implicitly via line breaks and indentation.
  --   This can be used for example to render Haskell @do@ blocks.
  --
  --   @BSExplicit@ displays blocks explicitly via characters for block start, block end, and item separation.
  --   This can be used for example to render C-style brace and semicolon delimited blocks with @BSExplicit '{' ';' '}'@.
  --
  --   Default: @BSImplicit@
  , _configBlockStyle :: BlockStyle
  }

data BlockStyle = BSImplicit | BSExplicit Char Char Char

instance Default Config where
  def = Config
    -- { configSpacesPerIndent = 2
    { _configMaxLineWidth    = 80
    , _configInitPrecedence  = -1
    , _configInitIndent      = 0
    , _configIndentChar      = ' '
    , _configIndentDepth     = 2
    , _configIndentAfterBreaks = 2
    , _configBlockStyle      = BSImplicit
    }

makeLenses ''Config

-- Pretty Printing -------------------------------------------------------------

-- | Render a 'Pretty' printable @a@ to 'String' using a 'Config', that
--   specifies how the @a@ should be rendered.
--
--   The following example uses the default configuration to render @1@:
--
--   > pretty def (1 :: Int)  -- evaluates to "1"
pretty :: Pretty a => Config → a → String
pretty c
  = concat
  . (`sepByL` "\n")
  . reverse
  . NE.toList
  . view text
  . flip execState (PrettySt (_configInitIndent c)
                             (_configInitPrecedence c)
                             AssocN
                             (_configMaxLineWidth c)
                             ("" :| []))
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
prettyPrint :: (MonadIO m, Pretty a) => Config → a → m ()
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

instance Pretty String where
  pp s' = do
    text . NE.headL %= (++s')
    s ← use $ text . NE.headL
    w ← use maxLineWidth
    when (w < length s) $ do
      let (s1, s2) = splitAt w s
      let (s12, s11) = both %~ reverse $ break (==' ') $ reverse s1
      let (line, rest)
            | all (== ' ') s11 = _1 %~ (s1++) $ break (==' ') s2
            | otherwise = (s11, s12 ++ s2)
      text . NE.headL .= line
      unless (all (== ' ') rest) $ indented $ indented $ do nl; pp rest

-- curLineHasSpace :: PrettyM Bool
-- curLineHasSpace = do
--   s ← use $ text . NE.headL
--   w ← use maxLineWidth
--   pure $ w > length s

-- trySplitAtLastSpace :: String → Maybe (String, String)
-- trySplitAtLastSpace s =
--   let (indent, rest) = splitIndent s
--   in case splitAtChar ' ' rest of
--       [] → Nothing
--       xs → Just (init xs, last xs)

-- splitAtChar :: Char → String → [String]
-- splitAtChar = go "" where
--   go xs = \case
--     c:cs | c == c0   → xs : go "" cs
--          | otherwise → go (xs++[c]) cs
--     []               → []

-- splitIndent :: String → (String, String)
-- splitIndent = undefined

-- instance Pretty Char where
--   pp c = curLineHasSpace >>= \case
--     True → text . NE.headL %~ (++[c])
--     False → do
--       carry ←
--       text %~ (carry ++ [c] :|)
--     case
--     when b $ do
--       let (s1, s2) = splitAt w s
--       let (s12, s11) = both %~ reverse $ break (==' ') $ reverse s1
--       let (line, rest)
--             | all (== ' ') s11 = _1 %~ (s1++) $ break (==' ') s2
--             | otherwise = (s11, s12 ++ s2)
--       text . NE.headL .= line
--       unless (all (== ' ') rest) $ indented $ indented $ do nl; pp rest

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
indent = indentation %= (+1)

-- | Decrement indentation level.
unindent :: PrettyM ()
unindent = indentation %= subtract 1

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
indented a = indent +> a +> unindent

addIndent :: PrettyM ()
addIndent = do
  i <- use indentation
  pp $ replicate (i*2) ' '

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
block' xs = indented $        xs `sepBy` nl

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
  replicate 5 c ++> s ++> replicate (w - (7 + length (pretty def s))) c +> "\n"


-- | Print a newline (line break).
nl :: PrettyM ()
nl = do
  text %= ("" NE.<|)
  addIndent

-- | Print a space.
sp :: PrettyM ()
sp = pp " "

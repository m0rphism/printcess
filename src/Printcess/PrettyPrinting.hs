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
  (+>),
  (++>),

  -- * Indentation
  indent, unindent, indented,

  -- * Associativity & Fixity
  assocL, assocR, assocN,
  left, right, inner, AssocAnn(..),

  -- * Folding Lists of @Pretty@ Things
  sepBy, interleaveL, interleaveR,
  block, block',
  ppList, ppSExp,

  -- * Folding Maps of @Pretty@ Things
  ppListMap, ppMap,

  -- * Misc
  ppParen,
  ppBar,

  -- * Constants
  nl, sp,

  -- * Lifted Type Classes
  Pretty1(..), Pretty2(..),

  -- * Reexports
  State, (.=),
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
    >   pp (EApp e1 e2) = assocL 9 $ L e1 ++> R e2
    >   pp (EAbs x e)   = assocR 0 $ "λ" +> I x +> "." ++> R e

    We print

    *   a variable @EVar x@ by printing the identifier string @x@.

    *   a function application @EApp e1 e2@ as a left-associative operator of
        fixity 9 ('assocL' @9@), where e1 is the left argument ('L') and @e2@ is
        the right argument ('R'). The ('++>') combinator separates its first
        argument with a space from its second argument.

    *   a function abstraction @EAbs x e@ as a right-associative operator of
        fixity 0 ('assocR' @0@), where @x@ is an inner
        argument ('I') and @e@ is the right argument ('R').
        The ('+>') combinator behaves as ('++>'), but without inserting a space.

    Then we define a simple test expression @e1@ representing @λx. λy. x y (x y)@

    > e1 :: Expr
    > e1 = EAbs "x" $ EAbs "y" $ EApp (EApp (EVar "x") (EVar "y"))
    >                                 (EApp (EVar "x") (EVar "y"))

    and pretty print it to 'String' using the 'pretty' function

    > s1, s2 :: String
    > s1 = pretty defConfig             e1    -- evaluates to "λx. λy. x y (x y)"
    > s2 = pretty (cMaxLineWidth .= 12) e1    -- evaluates to "λx. λy. x y
    >                                         --                   (x y)"
-}

-- Config ----------------------------------------------------------------------

-- | A 'Config' allows to specify various pretty printing options, e.g.
-- the maximum line width.
--
-- As the rendering functions, like 'pretty', take updates to an internal
-- default 'Config', only the lenses of the 'Config' fields are exported.
--
-- A custom 'Config' can be specified as in the following example:
--
-- > foo :: String
-- > foo = pretty config "foo bar baz"
-- >   where config = do cMaxLineWidth      .= 6
-- >                     cInitIndent        .= 2
-- >                     cIndentAfterBreaks .= 0
data Config = Config
  { _configMaxLineWidth      :: Int
  , _configInitPrecedence    :: Int
  , _configInitIndent        :: Int
  , _configIndentChar        :: Char
  , _configIndentDepth       :: Int
  , _configIndentAfterBreaks :: Int
  }

def :: Config
def = Config
  { _configMaxLineWidth      = 80
  , _configInitPrecedence    = -1
  , _configInitIndent        = 0
  , _configIndentChar        = ' '
  , _configIndentDepth       = 2
  , _configIndentAfterBreaks = 4
  }

-- | Leaves the default @Config@ unchanged and returns @()@.
--
-- Convenience function defined as:
--
-- > defConfig = pure ()
--
-- See example in 'pretty'.
defConfig :: State Config ()
defConfig = pure ()

makeLenses ''Config

-- | When a line gets longer, it is broken after the latest space,
--   that still allows the line to remain below this maximum.
--
--   If there is no such space, an over-long line with a single indented word is
--   printed.
--
--   This guarantees both progress and not to break identifiers into parts.
--
--   Default: @80@
cMaxLineWidth :: Lens' Config Int
cMaxLineWidth = configMaxLineWidth

-- | The character used for indentation.
--   Usually @' '@ for spaces or @'\t'@ for tabs.
--
--   Default: @' '@
cIndentChar :: Lens' Config Char
cIndentChar = configIndentChar

-- | How many 'cIndentChar' characters for one indentation level.
--
--   Default: @2@
cIndentDepth :: Lens' Config Int
cIndentDepth = configIndentDepth

-- | How many 'cIndentChar' characters to indent additionally, when a line break
--   occurs, because 'cMaxLineWidth' was exceeded.
--
--   Assuming the line to print has to be broken multiple times, the
--   indentation of all resulting lines, except the first one, is increased by this amount.
--   For example
--
--   > pretty (do cMaxLineWidth .= 8; cIndentAfterBreaks .= 4) "foo bar baz boo"
--   evaluates to
--
--   > foo bar
--   >     baz
--   >     boo
--
--   Default: @4@
cIndentAfterBreaks :: Lens' Config Int
cIndentAfterBreaks = configIndentAfterBreaks

-- | Indentation level to start pretty printing with.
--
--   Default: @0@
cInitIndent :: Lens' Config Int
cInitIndent = configInitIndent

-- | Precendence level to start pretty printing with.
--
--  Default: @(-1)@
cInitPrecedence :: Lens' Config Int
cInitPrecedence = configInitPrecedence

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
--   specifies how the @a@ should be rendered. For example
--
--   > pretty defConfig (1 :: Int)  -- evaluates to "1"
pretty
  :: Pretty a
  => State Config () -- ^ Updates for the default pretty printing 'Config'.
  -> a               -- ^ A 'Pretty' printable @a@.
  -> String          -- ^ The pretty printed @a@.
pretty c
  = concat
  . (`sepByL` "\n")
  . reverse
  . NE.toList
  . view text
  . (`execState` stFromConfig (execState c def))
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
prettyPrint
  :: (MonadIO m, Pretty a)
  => State Config () -- ^ Updates for the default pretty printing 'Config'.
  -> a               -- ^ A 'Pretty' printable @a@.
  -> m ()            -- ^ An 'IO' action pretty printing the @a@ to @stdout@.
prettyPrint c =
  liftIO . putStrLn . pretty c

-- Type Classes ----------------------------------------------------------------

-- | Instanciating this class for a type, declares how values of that type
-- should be pretty printed.
--
-- As pretty printing may depend on some context, e.g. the current indentation
-- level, a 'State' monad for pretty printing ('PrettyM') is used.
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

-- | In contrast to 'Show', @"foo"@ is printed as @"foo"@ and not @"\\"foo\\""@.
-- Most of the other instances are defined in terms of this instance.
-- If the 'String' contains newline characters (@'\n'@), indentation is inserted
-- automatically afterwards.
-- If the current line gets too long, it is automatically broken.
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
        let f | first     = indentedBy i
              | otherwise = id
        f $ do nl; ppLine False $ dropWhile isWS $ wordRest ++ lineRest

-- | In contrast to 'Show', @\'c\'@ is printed as @"c"@ and not @"\'c\'"@.
-- Implemented as: @pp = pp . (:"")@
instance Pretty Char   where pp = pp . (:"")

-- | Behaves like 'Show': @1@ is printed to @"1"@.
-- Implemented as: @pp = pp . show@
instance Pretty Int    where pp = pp . show

-- | Behaves like 'Show': @1.2@ is printed to @"1.2"@.
-- Implemented as: @pp = pp . show@
instance Pretty Float  where pp = pp . show

-- | Behaves like 'Show': @1.2@ is printed to @"1.2"@.
-- Implemented as: @pp = pp . show@
instance Pretty Double where pp = pp . show

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pp = foldl pp' (pp "") . M.toList where
    pp' s (k, v) = s +> k ++> "=>" ++> indented v +> nl

-- | The 'Pretty1' type class lifts 'Pretty' printing to unary type constructors.
--   It can be used in special cases to abstract over type constructors which
--   are 'Pretty' printable for any 'Pretty' printable type argument.
class Pretty1 f where
  pp1 :: Pretty a => f a → PrettyM ()
  default pp1 :: Pretty (f a) => f a -> PrettyM ()
  pp1 = pp

-- | The 'Pretty2' type class lifts 'Pretty' printing to binary type constructors.
--   It can be used in special cases to abstract over type constructors which
--   are 'Pretty' printable for any 'Pretty' printable type arguments.
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
-- instance Pretty (PrettyM a) where pp = (>> return ())

-- instance a ~ () => IsString (PrettyM ()) where
--   fromString = pp

instance a ~ () => Monoid (PrettyM a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- Basic Combinators -----------------------------------------------------------

-- | Print two 'Pretty' printable things in sequence.
--
--   Example:
--
--   > pretty defConfig $ "x" +> 1  -- ↪ "x1"
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
--   > pretty defConfig $ "x" ++> 1  -- ↪ "x 1"
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

-- | Increment indentation level.
indentBy :: Int → PrettyM ()
indentBy = (indentation +=)

-- | Decrement indentation level.
unindentBy :: Int → PrettyM ()
unindentBy = (indentation -=)

-- | Print an @a@ using an incremented indentation after newlines.
--
--   Example:
--
--   > pretty defConfig $ "while (true) {" +>
--   >              indented (nl +> "f();" +> nl +> "g();") +>
--   >              nl +> "}"
--   > ≡ pretty defConfig $ "while (true) {" +>
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

indentedBy :: Pretty a => Int → a → PrettyM ()
indentedBy i a = do indentBy i; pp a; unindentBy i

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
--   > pretty defConfig $ []         `sepBy` ","  -- ↪ ""
--   > pretty defConfig $ ["x"]      `sepBy` ","  -- ↪ "x"
--   > pretty defConfig $ ["x", "y"] `sepBy` ","  -- ↪ "x,y"
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
--   > pretty defConfig $ interleaveL "," []          -- ↪ ""
--   > pretty defConfig $ interleaveL "," ["x"]       -- ↪ ",x"
--   > pretty defConfig $ interleaveL "," ["x", "y"]  -- ↪ ",x,y"
interleaveL :: (Pretty a, Pretty b) => a → [b] → PrettyM ()
interleaveL a bs = fold $ interleaveL' (pp a) (pp <$> bs)

-- | Put an @a@ after each element of a @[b]@ and then print them in sequence.
--
--   Examples:
--
--   > pretty defConfig $ interleaveR "," []          -- ↪ ""
--   > pretty defConfig $ interleaveR "," ["x"]       -- ↪ "x,"
--   > pretty defConfig $ interleaveR "," ["x", "y"]  -- ↪ "x,y,"
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
-- > pretty defConfig $ "do" ++> block ["putStrLn hello", "putStrLn world"]
-- > -- ↪ "do
-- > --      putStrLn hello
-- > --      putStrLn world"
block :: Pretty a => [a] → PrettyM ()
block  xs = indented $ nl +> (xs `sepBy` nl)

-- | Same as 'block', but starts the block on the current line.
--
-- Example:
--
-- > pretty defConfig $ "do" ++> block' ["putStrLn hello", "putStrLn world"]
-- > -- ↪ "do putStrLn hello
-- > --       putStrLn world"
block' :: Pretty a => [a] → PrettyM ()
block' xs = indentedToCurPos $ xs `sepBy` nl

-- | Print a @[a]@ similar to its 'Show' instance.
--
--   Example:
--
--   > pretty defConfig $ ppList [ "x", "y" ]  -- ↪ "[ x, y ]"
--
--   Convenience function, defined as:
--
--   > ppList ps = "[" ++> (ps `sepBy` ", ") ++> "]"
ppList :: Pretty a => [a] → PrettyM ()
ppList ps = "[" ++> (ps `sepBy` ", ") ++> "]"

-- | Print a list map @[(k,v)]@ as 'ppList', but render @(k,v)@ pairs as @"k → v"@.
--
--   Example:
--
--   > pretty defConfig $ ppListMap [ ("k1", "v1"), ("k2", "v2") ]  -- ↪ "[ k1 → v1, k2 → v2 ]"
--
--   Convenience function, defined as:
--
--   > ppListMap = block . map (\(a,b) → a ++> "→" ++> b)
ppListMap :: (Pretty a, Pretty b) => [(a, b)] → PrettyM ()
ppListMap = block . map (\(a,b) → a ++> "→" ++> b)

-- | Print a @Data.Map@ in the same way as 'ppListMap'.
ppMap :: (Pretty a, Pretty b) => M.Map a b → PrettyM ()
ppMap = ppListMap . M.assocs

-- | Print an @a@ in parentheses.
--
--   Example:
--
--   > pretty defConfig $ ppParen "foo"  -- ↪ "(foo)"
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
--   > pretty defConfig $ ppSExp ["+", "2", "3"]  -- ↪ "(+ 2 3)"
--
--   Convenience function, defined as:
--
--   > ppSExp = ppParen . (`sepBy` sp)
ppSExp ∷ Pretty b ⇒ [b] → PrettyM ()
ppSExp = ppParen . (`sepBy` sp)

-- | Print a horizontal bar consisting of a 'Char' as long as 'cMaxLineWidth'.
--   The horizontal bar has a title 'String' printed at column 6.
--
--   Example:
--
--   > pretty defConfig $ ppBar '-' "Foo"
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

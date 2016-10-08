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
  Config(..), configMaxLineWidth, configInitPrecedence, configInitIndent,
  Data.Default.def,
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
  -- * Composite Combinators
  sepBy, interleaveL, interleaveR,
  block, block',
  maybePrint, ifPrint,
  ppList, ppListMap, ppMap, ppParen, ppSExp,
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
-- creating a config with maximum line width of @70@ and an initial indentation
-- level of @2@:
--
-- > import Control.Lens ((&), (.~))
-- > import Data.Default (def)
-- >
-- > foo :: String
-- > foo = pretty config "foo"
-- >   where config = def & configMaxLineWidth .~ 70
-- >                      & configInitIndent   .~  2
data Config = Config
  -- { configSpacesPerIndent :: Int
  {
  -- | When a line gets longer, it is broken after the latest space,
  --   that still allows the line to remain below this maximum.
  --
  --  Default: 80
    _configMaxLineWidth    :: Int
  -- | Precendence level to start pretty printing with.
  --
  --  Default: (-1)
  , _configInitPrecedence  :: Int
  -- | Indentation level to start pretty printing with.
  --
  --  Default: 0
  , _configInitIndent      :: Int
  }

instance Default Config where
  def = Config
    -- { configSpacesPerIndent = 2
    { _configMaxLineWidth    = 80
    , _configInitPrecedence  = -1
    , _configInitIndent      = 0
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
pretty c = concat . (`sepByL` "\n") . reverse . NE.toList . view text
          . flip execState (PrettySt (_configInitIndent c)
                                     (_configInitPrecedence c)
                                     AssocN
                                     (_configMaxLineWidth c)
                                     ("" :| []))
          . runPrettyM . pp . (addIndent +>)

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
class Pretty a where
  -- | Pretty print an @a@ as a 'PrettyM' action.
  pp :: a → PrettyM ()

instance Pretty String where pp = write
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
--   fromString = write
-- instance Pretty (PrettyM a) where pp = (>> return ())
instance a ~ () => Monoid (PrettyM a) where
  mempty = pure mempty
  mappend = liftA2 mappend


-- Basic Combinators -----------------------------------------------------------

-- | Print two 'Pretty' printable things in sequence.
--
--   > a +> b = pp a >> pp b
(+>) :: (Pretty a, Pretty b) => a → b → PrettyM ()
a +> b = pp a >> pp b

-- | Print two 'Pretty' printable things in sequence, separated by a space.
--
--   > a ++> b = a +> " " +> b
(++>) :: (Pretty a, Pretty b) => a → b → PrettyM ()
a ++> b = a +> sp +> b

-- TODO: Does this need to be exported? What does it do?
-- Append a string to the printed text, potentially causing a line break if
-- the maximum text width gets exceeded.
write :: String → PrettyM ()
-- FIXME: was there a reason for this? o.O
-- write = (`sepByA_` nl) . map write' . (`splitAtDelim` '\n')
write = write'

-- TODO: Does this need to be exported? What does it do?
write' :: String → PrettyM ()
write' s = do
  text . NE.headL %= (++s)
  ensureLineWidth

ensureLineWidth :: PrettyM ()
ensureLineWidth = do
  s ← use $ text . NE.headL
  w ← use maxLineWidth
  when (w < length s) $ do
    let (s1, s2) = splitAt w s
    let (s12, s11) = both %~ reverse $ break (==' ') $ reverse s1
    let (line, rest)
         | all (== ' ') s11 = _1 %~ (s1++) $ break (==' ') s2
         | otherwise = (s11, s12 ++ s2)
    text . NE.headL .= line
    unless (all (== ' ') rest) $ indented $ indented $ do nl; write' rest

-- Indentation -----------------------------------------------------------------

-- | Increment indentation level.
indent :: PrettyM ()
indent = indentation %= (+1)

-- | Decrement indentation level.
unindent :: PrettyM ()
unindent = indentation %= subtract 1

-- | Print an @a@ using an incremented indentation after newlines.
indented :: Pretty a => a → PrettyM ()
indented a = indent +> a +> unindent

addIndent :: PrettyM ()
addIndent = do
  i <- use indentation
  write' $ replicate (i*2) ' '

-- Associativity & Fixity ------------------------------------------------------

withPrecedence :: (Assoc, Int) → PrettyM () → PrettyM ()
withPrecedence (a, p) ma = do
  p' ← use precedence
  a' ← use assoc
  precedence .= p
  assoc .= a
  if | p' == p && a' == a && a /= AssocN → ma
     | p' < p    → ma
     | otherwise → do write' "("; ma; write' ")"
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

-- | If @Nothing@, print @""@; if @Just x@ print @x +> a@.
maybePrint :: (Pretty a, Pretty b) => a → Maybe b → PrettyM ()
maybePrint _ Nothing  = pp ""
maybePrint p (Just a) = p +> a

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
sp = write " "


-- Internal --------------------------------------------------------------------

-- Instances for Kallisti.Functor Types ----------------------------------------

-- NOTE: removed because of dependencies to Functor Combinators.

-- -- instance (Pretty (f a), Pretty (g a)) => Pretty ((f :+: g) a) where
-- --   pp (F a) = pp a
-- --   pp (G a) = pp a
-- instance (Pretty1 f, Pretty1 g) => Pretty1 (f :.: g) where
--   pp1 (FG fga) = pp1 fga
-- instance (Pretty1 f, Pretty1 g) => Pretty1 (f :+: g) where
--   pp1 (F fa) = pp1 fa
--   pp1 (G ga) = pp1 ga
-- instance Pretty a => Pretty1 (Lit a) where pp1 (Lit a) = pp a
-- instance Pretty1 IdF                 where pp1 (IdF a) = pp a
-- instance Pretty Void                 where pp e = case e of

-- instance Pretty1 f => Pretty (Fix f) where
--   pp = pp1 . unFix

-- data E = EVar String
--        | ELam String E
--        | EApp E E
--        | EAdd E E
--        | EMul E E
--        | EIf E E E
--        | EBlock [E] E

-- instance Pretty E where
--   pp = \case
--     EVar i      → pp i
--     ELam i e    → assocR 0 $ "λ" +> i +> "." ++> R e
--     EApp e1 e2  → assocL 9 $ L e1 ++> R e2
--     EAdd e1 e2  → assocR 5 $ L e1 ++> "+" ++> R e2
--     EMul e1 e2  → assocR 6 $ L e1 ++> "·" ++> R e2
--     EIf e1 e2 e3 → assocR 6 $ "if" ++> L e1 ++> "then" ++> L e2 ++> "else" ++> R e3
--     EBlock es e → "do" ++> block es ++> "in" ++> e

-- test :: IO ()
-- test = do
--   prettyPrint' 0 0 5 "12 3"
--   prettyPrint' 0 0 5 "12 34"
--   prettyPrint' 0 0 5 "12 345"
--   prettyPrint' 0 0 5 "1234"
--   prettyPrint' 0 0 5 "12345"
--   prettyPrint' 0 0 5 "123456"
--   prettyPrint $ EIf (EVar "b") (EVar "b") (EVar "b")
--   prettyPrint $ EIf (EIf (EVar "b") (EVar "b") (EVar "b"))
--                     (EIf (EVar "b") (EVar "b") (EVar "b"))
--                     (EIf (EVar "b") (EVar "b") (EVar "b"))
--   prettyPrint' 0 0 20 $ ELam "fooo1" $ ELam "baaaar2" $ ELam "baaaaaz3" $ EVar "xooooo4"
--   prettyPrint $ ELam "x" $ ELam "y" $ EAdd (EMul (EVar "x") (EVar "x")) (EMul (EVar "x") (EVar "x"))
--   prettyPrint $ ELam "x" $ ELam "y" $ EMul (EAdd (EVar "x") (EVar "x")) (EAdd (EVar "x") (EVar "x"))
--   prettyPrint $ ELam "x" $ ELam "y" $ EMul (EAdd (EMul (EVar "x") (EVar "x")) (EVar "x")) (EVar "x")
--   prettyPrint $ ELam "x" $ ELam "y" $ EAdd (EAdd (EVar "x") (EVar "x")) (EAdd (EVar "x") (EVar "x"))
--   prettyPrint $ ELam "x" $ ELam "y" $ foldl1 EAdd (replicate 10 $ EVar "x")
--   prettyPrint $ ELam "x" $ ELam "y" $ foldr1 EAdd (replicate 10 $ EVar "x")
--   prettyPrint $ ELam "x" $ ELam "y" $ foldl1 EApp (replicate 10 $ EVar "x")
--   prettyPrint $ ELam "x" $ ELam "y" $ foldr1 EApp (replicate 10 $ EVar "x")
--   prettyPrint $ EApp (ELam "x" $ EVar "x") (ELam "y" $ EVar "y")
--   prettyPrint $ EApp (EVar "x") $ EApp (EVar "x") $ EBlock
--     [ EApp (ELam "x" $ EVar "x") (ELam "y" $ EVar "y")
--     , EVar "y"
--     , ELam "x" $ ELam "y" $ EAdd (EMul (EVar "x") (EVar "x")) (EMul (EVar "x") (EVar "x"))
--     ] (EVar "X")

{-# LANGUAGE
  DefaultSignatures, FlexibleInstances, GeneralizedNewtypeDeriving,
  KindSignatures, LambdaCase, MultiWayIf, TemplateHaskell, UnicodeSyntax
#-}

module Printcess.Core where

import Control.Monad.State.Lazy
import Control.Lens
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

import Printcess.Config

-- Associativity ---------------------------------------------------------------

data Assoc = AssocN | AssocL | AssocR
  deriving (Eq, Ord, Read, Show)

-- Pretty Printing State -------------------------------------------------------

data PrettySt = PrettySt
  { _indentation       :: Int
  , _precedence        :: Int
  , _assoc             :: Assoc
  , _maxLineWidth      :: Maybe Int
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
  . (`sepByList` "\n")
  . reverse
  . NE.toList
  . view text
  . (`execState` stFromConfig (execState c def))
  . runPrettyM
  . pp

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
--
-- A default implementation is provided copying behavior from a 'Show' instance.
-- This can be convenient for deriving 'Pretty', e.g. for base types or
-- debugging. The default implementation is defined by @pp = pp . show@.
class Pretty a where
  -- | Pretty print an @a@ as a 'PrettyM' action.
  pp :: a → PrettyM ()

  default
    pp :: Show a => a → PrettyM ();
    pp = pp . show

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

dropWhileEnd :: (a → Bool) → [a] → [a]
dropWhileEnd f = reverse . dropWhile f . reverse

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
    go (s:ss) = do ppLine True s; text %= ("" NE.<|); go ss

    ppLine :: Bool -> String -> PrettyM ()
    ppLine first s = do
      oldLine ← use $ text . head1L
      when (null oldLine) addIndent
      text . head1L %= (++s)
      curLine ← use $ text . head1L
      -- We have to allow at least indentation + 1 word, otherwise indenting after
      -- line break due to line continuation can cause infinite loop
      mMaxLineLength ← use maxLineWidth
      forM_ mMaxLineLength $ \maxLineLength → do
        maxLineLength' ← max <$> charsBeforeWordM 1 <*> pure maxLineLength
        when (length curLine > maxLineLength') $ do
          let (curLine', lineRest) = splitAt maxLineLength curLine -- length s1 == maxLineLength
          let (wordRest, curLine'')
                | isNoWS (head lineRest)
                  = both %~ reverse $ break (==' ') $ reverse curLine'
                | otherwise
                  = ("", curLine')
          text . head1L .= dropWhileEnd isWS curLine''
          -- Increase indentation once after the first forced line break, this results into:
          --   this line was too long
          --       still the first line
          --       it won't stop
          i ← use indentAfterBreaks
          let f | first     = indentedByChars i
                | otherwise = id
          f $ do text %= ("" NE.<|); ppLine False $ dropWhile isWS $ wordRest ++ lineRest

-- | In contrast to 'Show', @\'c\'@ is printed as @"c"@ and not @"\'c\'"@.
instance Pretty Char   where pp = pp . (:"")

-- | Behaves like 'Show': @1@ is printed to @"1"@.
instance Pretty Int

-- | Behaves like 'Show': @1.2@ is printed to @"1.2"@.
instance Pretty Float

-- | Behaves like 'Show': @1.2@ is printed to @"1.2"@.
instance Pretty Double

-- -- | Print a map @M.fromList [("k1","v1"), ("k2","v2")]@
-- -- as @"[ k1 → v1, k2 → v2 ]"@.
-- instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
--   pp = foldl pp' (pp "") . M.toList where
--     pp' s (k, v) = s +> k ~> "=>" ~> indented v +> nl

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
-- 'pretty' or 'prettyPrint'.
--
-- 'PrettyM' is internally a 'State' monad manipulating a 'Config' and a list of
-- pretty printed lines.
--
-- Most of the combinators from this library take values of 'Pretty' printable types,
-- convert them to @'PrettyM' ()@ actions using 'pp', and combine the actions in
-- some way resulting in a new @'PrettyM' ()@ action.
newtype PrettyM a = PrettyM { runPrettyM :: State PrettySt a }
  deriving (Functor, Applicative, Monad, MonadState PrettySt)

-- | This instance makes it possible to nest operators like @('+>')@.
-- Implemented as: @pp = id@
instance Pretty (PrettyM ()) where pp = id


sepByList :: [[a]] → [a] → [[a]]
sepByList []  _    = []
sepByList [s] _    = [s]
sepByList (s:ss) s' = s : s' : sepByList ss s'

addIndent :: PrettyM ()
addIndent = do
  i <- use indentation
  c <- use indentChar
  text . head1L %= (++ replicate i c)

-- Indentation -----------------------------------------------------------------

indentByChars
  :: Int
  -> PrettyM ()
indentByChars =
  (indentation +=)

indentByLevels
  :: Int
  -> PrettyM ()
indentByLevels i =
  (indentation +=) . (i *) =<< use indentDepth

-- | Print an @a@ with indentation increased by a certain amount of
--   'cIndentChar' characters.
--
--   Example:
--
--   > pretty defConfig $
--   >   "while (true) {" \>
--   >   indentedByChars 2 ("f();" \> "g();") \>
--   >   "}"
--   > -- ↪ "while (true) {
--   > --      f();
--   > --      g();
--   > --    }"
indentedByChars
  :: Pretty a
  => Int         -- ^ Number of characters to increase indentation.
  -> a           -- ^ A 'Pretty' printable @a@
  -> PrettyM ()  -- ^ An action printing the @a@ with increased indentation.
indentedByChars i a = do
  indentByChars i
  pp a
  indentByChars (-i)

-- | Same as 'indentedByChars' but increases indentation in 'cIndentDepth' steps.
indentedBy
  :: Pretty a
  => Int          -- ^ Number of indentation levels to increase.
                  --   One indentation level consists of 'cIndentDepth' characters.
  -> a            -- ^ A 'Pretty' printable @a@
  -> PrettyM ()   -- ^ An action printing the @a@ with increased indentation.
indentedBy i a = do
  indentByLevels i
  pp a
  indentByLevels (-i)

-- | Convenience function defined as:
--
-- > indented = indentedBy 1
indented
  :: Pretty a
  => a           -- ^ A 'Pretty' printable @a@
  -> PrettyM ()  -- ^ An action printing the @a@ indented 1 level deeper.
indented =
  indentedBy 1

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

-- | The constructors of this type can be used as short forms of 'left',
-- 'right', and 'inner'.
data AssocAnn a
  = L a -- ^ Print an @a@ as the left  argument of a mixfix operator (behaves like 'left').
  | R a -- ^ Print an @a@ as the right argument of a mixfix operator (behaves like 'right').
  | I a -- ^ Print an @a@ as the inner argument of a mixfix operator (behaves like 'inner').
  deriving (Eq, Ord, Read, Show)

instance Pretty1 AssocAnn

-- | Let the associativity annotations for arguments ('L', 'R', 'I')
-- behave as the 'left', 'right', and 'inner' functions.
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

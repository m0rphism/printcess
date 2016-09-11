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
  -- * Types
  PrettyM, Pretty(..), Config(..), defConfig,
  -- * Eliminations
  pretty, pretty',
  prettyPrint, prettyPrint',
  -- * Basic Combinators
  (+>),
  (++>),
  indent, unindent, indented, addIndent,
  assocL, assocR, assocN,
  left, right, inner, AssocAnn(..),
  write, write',
  -- * Composite Combinators
  sepBy, sepBySP, sepByNL, sepByA_, sepByA, sepByL,
  interleaveL, interleaveR,
  splitAtDelim,
  nl, sp,
  block, block',
  maybePrint, ifPrint,
  enclose,
  ppList, ppListMap, ppMap, ppParen, ppSExp,
  pps,
  ppBar,
  tracePretty, tracePrettyId, tracePrettyM,
  ) where

import Control.Applicative
import Control.Monad.State.Lazy
import Control.Lens
import Data.Bifunctor
import Data.Foldable
import Data.String
import Data.List (takeWhile, dropWhile, dropWhileEnd)
import qualified Data.Map as M
import Debug.Trace
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

data Config = Config
  -- { configSpacesPerIndent :: Int
  { configMaxLineWidth    :: Int
  , configInitPrecedence  :: Int
  , configInitIndent      :: Int
  }

defConfig :: Config
defConfig = Config
  -- { configSpacesPerIndent = 2
  { configMaxLineWidth    = 80
  , configInitPrecedence  = -1
  , configInitIndent      = 0
  }

data Assoc = AssocN | AssocL | AssocR
  deriving (Eq, Ord, Read, Show)

data AssocAnn a = L a | R a | I a
  deriving (Eq, Ord, Read, Show)

declareLenses [d|
  data PrettySt = PrettySt
    { indentation  :: Int
    , precedence   :: Int
    , assoc        :: Assoc
    , maxLineWidth :: Int
    , text         :: NE.NonEmpty String
    }
  |]

newtype PrettyM a = PrettyM { runPrettyM :: State PrettySt a }
  deriving (Functor, Applicative, Monad, MonadState PrettySt)

-- instance a ~ () => IsString (PrettyM ()) where
--   fromString = write

class Pretty a              where pp :: a → PrettyM ()

instance Pretty1 AssocAnn
instance Pretty a => Pretty (AssocAnn a) where
  pp = \case
    L a → left a
    R a → right a
    I a → inner a

class Pretty2 (f :: * → * → *) where
  pp2 :: (Pretty a, Pretty b) => f a b → PrettyM ()
  default pp2 :: Pretty (f a b) => f a b -> PrettyM ()
  pp2 = pp

class Pretty1 f where
  pp1 :: Pretty a => f a → PrettyM ()
  default pp1 :: Pretty (f a) => f a -> PrettyM ()
  pp1 = pp

instance Pretty (PrettyM ()) where pp = (>> return ())
instance Pretty String      where pp = write
-- instance Pretty (PrettyM a) where pp = (>> return ())
-- instance Pretty String      where pp = write

instance Pretty Char        where pp = pp . (:[])
instance Pretty Int         where pp = pp . show
instance Pretty Float       where pp = pp . show
instance Pretty Double      where pp = pp . show

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pp = foldl pp' (pp "") . M.toList where
    pp' s (k, v) = s +> k ++> "=>" ++> indented v +> nl

instance a ~ () => Monoid (PrettyM a) where
  mempty = pure mempty
  mappend = liftA2 mappend

(+>) :: (Pretty a, Pretty b) => a → b → PrettyM ()
a +> b = pp a >> pp b

(++>) :: (Pretty a, Pretty b) => a → b → PrettyM ()
a ++> b = a +> sp +> b

prettyPrint :: Pretty a => a → IO ()
prettyPrint = putStrLn . pretty

pretty :: Pretty a => a → String
pretty = pretty' defConfig

pretty' :: Pretty a => Config → a → String
pretty' c = concat . (`sepByL` "\n") . reverse . NE.toList . view text
          . flip execState (PrettySt (configInitIndent c)
                                     (configInitPrecedence c)
                                     AssocN
                                     (configMaxLineWidth c)
                                     ("" :| []))
          . runPrettyM . pp . (addIndent +>)

prettyPrint' :: Pretty a => Config → a → IO ()
prettyPrint' c = putStrLn . pretty' c

indent :: PrettyM ()
indent = indentation %= (+1)

unindent :: PrettyM ()
unindent = indentation %= subtract 1

indented :: Pretty a => a → PrettyM ()
indented a = indent +> a +> unindent

addIndent :: PrettyM ()
addIndent = do
  i <- use indentation
  write' $ replicate (i*2) ' '

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

assocL :: Pretty a => Int → a → PrettyM ()
assocL i = withPrecedence (AssocL, i) . pp
assocR :: Pretty a => Int → a → PrettyM ()
assocR i = withPrecedence (AssocR, i) . pp
assocN :: Pretty a => Int → a → PrettyM ()
assocN i = withPrecedence (AssocN, i) . pp

left :: Pretty a => a → PrettyM ()
left = assocDir AssocL

right :: Pretty a => a → PrettyM ()
right = assocDir AssocR

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

(|-) ∷ (Assoc, Int) → PrettyM () → PrettyM ()
(|-) = withPrecedence; infix 1 |-

write :: String → PrettyM ()
write = (`sepByA_` nl) . map write' . (`splitAtDelim` '\n')

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

sepBy :: (Pretty a, Pretty b) => [a] → b → PrettyM ()
sepBy as s = sepByA_ (map pp as) (pp s)

sepBySP :: (Pretty a) => [a] → PrettyM ()
sepBySP = (`sepBy` sp)

sepByNL :: (Pretty a) => [a] → PrettyM ()
sepByNL = (`sepBy` nl)

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

interleaveL :: (Pretty a, Pretty b) => a → [b] → PrettyM ()
interleaveL a bs = fold $ interleaveL' (pp a) (pp <$> bs)
interleaveR :: (Pretty a, Pretty b) => a → [b] → PrettyM ()
interleaveR a bs = fold $ interleaveR' (pp a) (pp <$> bs)

interleaveL' :: a → [a] → [a]
interleaveL' s = foldl (\xs x → xs ++ [s,x]) []
interleaveR' :: a → [a] → [a]
interleaveR' s = foldl (\xs x → xs ++ [x,s]) []

splitAtDelim :: String → Char → [String]
splitAtDelim s' c = go [] s' where
  go s [] = [reverse s]
  go s (x:xs) | x == c    = reverse s : go "" xs
              | otherwise = go (x:s) xs

nl :: PrettyM ()
nl = do
  text %= ("" NE.<|)
  addIndent

sp :: PrettyM ()
sp = write " "

block, block' :: Pretty a => [a] → PrettyM ()
block  xs = indented $ nl +> (xs `sepBy` nl)
block' xs = indented $        xs `sepBy` nl

maybePrint :: (Pretty a, Pretty b) => a → Maybe b → PrettyM ()
maybePrint _ Nothing  = pp ""
maybePrint p (Just a) = p +> a

ifPrint :: Pretty a => Bool → a → PrettyM ()
ifPrint True  a = pp a
ifPrint False _ = pp ""

enclose :: (Pretty a, Pretty b, Pretty c) => a → b → [c] → [PrettyM ()]
enclose a b = fmap (\c → a +> c +> b)

ppList :: Pretty a => [a] → PrettyM ()
ppList ps = "[" ++> (ps `sepBy` ", ") ++> "]"

ppListMap :: (Pretty a, Pretty b) => [(a, b)] → PrettyM ()
ppListMap = block . map (\(a,b) → a ++> "→" ++> b)

ppMap :: (Pretty a, Pretty b) => M.Map a b → PrettyM ()
ppMap = ppListMap . M.assocs

ppParen ∷ Pretty b ⇒ b → PrettyM ()
ppParen x = "(" +> x +> ")"

ppSExp ∷ Pretty b ⇒ [b] → PrettyM ()
ppSExp = ppParen . sepBySP

pps ∷ Pretty a => [a] → [PrettyM ()]
pps = fmap pp

ppBar ∷ Pretty a => Char → a → PrettyM ()
ppBar c s = do
  w ← use maxLineWidth
  replicate 5 c ++> s ++> replicate (w - (7 + length (pretty s))) c +> "\n"

tracePretty :: Pretty a => a → b → b
tracePretty = trace . pretty

tracePrettyId :: Pretty a => a → a
tracePrettyId x = trace (pretty x) x

tracePrettyM :: (Monad m , Pretty a) => a → m ()
tracePrettyM = traceM . pretty

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

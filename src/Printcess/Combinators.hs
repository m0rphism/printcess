{-# LANGUAGE
  UnicodeSyntax
#-}

module Printcess.Combinators where

import Control.Lens
import qualified Data.Map as M

import Printcess.Core

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
infixr 5 +>
(+>) :: (Pretty a, Pretty b) => a → b → PrettyM ()
a +> b = pp a >> pp b

-- | Print two 'Pretty' printable things in sequence, separated by a space.
--
--   Example:
--
--   > pretty defConfig $ "x" ~> 1  -- ↪ "x 1"
--
--   Convenience function, defined as
--
--   > a ~> b = a +> " " +> b
infixr 4 ~>
(~>) :: (Pretty a, Pretty b) => a → b → PrettyM ()
a ~> b = a +> sp +> b

-- | Print two 'Pretty' printable things in sequence, separated by a newline.
--
--   Example:
--
--   > pretty defConfig $ "x" \> 1  -- ↪ "x
--   >                                    1"
--
--   Convenience function, defined as
--
--   > a \> b = a +> "\n" +> b
infixr 3 \>
(\>) :: (Pretty a, Pretty b) => a → b → PrettyM ()
a \> b = a +> nl +> b

-- Composite Combinators -------------------------------------------------------

-- | Print an @a@ between each @b@.
--
--   Examples:
--
--   > pretty defConfig $ []         `sepBy` ","  -- ↪ ""
--   > pretty defConfig $ ["x"]      `sepBy` ","  -- ↪ "x"
--   > pretty defConfig $ ["x", "y"] `sepBy` ","  -- ↪ "x,y"
sepBy :: (Pretty a, Pretty b) => [b] → a → PrettyM ()
sepBy as s = sepByA_ (map pp as) (pp s)

-- | Print an @a@ before each @b@.
--
--   Examples:
--
--   > pretty defConfig $ []         `sepByL` ","  -- ↪ ""
--   > pretty defConfig $ ["x"]      `sepByL` ","  -- ↪ ",x"
--   > pretty defConfig $ ["x", "y"] `sepByL` ","  -- ↪ ",x,y"
sepByL :: (Pretty a, Pretty b) => [b] → a → PrettyM ()
sepByL bs a = foldl (>>) (pure ()) $ map pp bs `sepByL'` pp a

-- | Print an @a@ after each @b@.
--
--   Examples:
--
--   > pretty defConfig $ []         `sepByR` ","  -- ↪ ""
--   > pretty defConfig $ ["x"]      `sepByR` ","  -- ↪ "x,"
--   > pretty defConfig $ ["x", "y"] `sepByR` ","  -- ↪ "x,y,"
sepByR :: (Pretty a, Pretty b) => [b] → a → PrettyM ()
sepByR bs a = foldl (>>) (pure ()) $ map pp bs `sepByR'` pp a

sepByA :: Applicative f => [f a] → f a → f [a]
sepByA []     _ = pure []
sepByA [a]    _ = (:[]) <$> a
sepByA (a:as) s = (\x y z → x:y:z) <$> a <*> s <*> sepByA as s

sepByA_ :: Applicative f => [f a] → f a → f ()
sepByA_ as s = () <$ sepByA as s

sepByL', sepByR' :: [a] → a → [a]
sepByL' xs0 s = foldl (\xs x → xs ++ [s,x]) [] xs0
sepByR' xs0 s = foldl (\xs x → xs ++ [x,s]) [] xs0

-- | Print a @[a]@ as a block, meaning that the indentation level is
-- increased, and each @a@ is printed on a single line.
--
-- Example:
--
-- > pretty defConfig $ "do" ~> block ["putStrLn hello", "putStrLn world"]
-- > -- ↪ "do
-- > --      putStrLn hello
-- > --      putStrLn world"
block :: Pretty a => [a] → PrettyM ()
block  xs = indented $ nl +> (xs `sepBy` nl)

-- | Same as 'block', but starts the block on the current line.
--
-- Example:
--
-- > pretty defConfig $ "do" ~> block' ["putStrLn hello", "putStrLn world"]
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
--   > ppList ps = "[" ~> (ps `sepBy` ", ") ~> "]"
ppList :: Pretty a => [a] → PrettyM ()
ppList ps = "[" ~> (ps `sepBy` ", ") ~> "]"

-- | Print a list map @[(k,v)]@ as 'ppList', but render @(k,v)@ pairs as @"k → v"@.
--
--   Example:
--
--   > pretty defConfig $ ppListMap [ ("k1", "v1"), ("k2", "v2") ]  -- ↪ "[ k1 → v1, k2 → v2 ]"
--
--   Convenience function, defined as:
--
--   > ppListMap = block . map (\(a,b) → a ~> "→" ~> b)
ppListMap :: (Pretty a, Pretty b) => [(a, b)] → PrettyM ()
ppListMap = block . map (\(a,b) → a ~> "→" ~> b)

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

-- | Print a horizontal bar consisting of a 'Char' as long as 'cMaxLineWidth'
--   (or 80 if it is @Nothing@). The horizontal bar has a title 'String' printed
--   at column 6.
--
--   Example:
--
--   > pretty defConfig $ ppBar '-' "Foo"
--   > -- ↪ "----- Foo -------------------------------…"
ppBar ∷ Pretty a => Char → a → PrettyM ()
ppBar c s = do
  w ← maybe 80 id <$> use maxLineWidth
  replicate 5 c ~> s ~> replicate (w - (7 + length (pretty (pure ()) s))) c +> "\n"


-- | Print a newline (line break).
nl :: PrettyM ()
nl = pp "\n"

-- | Print a space.
sp :: PrettyM ()
sp = pp " "

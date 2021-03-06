{-# LANGUAGE UnicodeSyntax #-}

module Printcess.Combinators where

import Control.Lens
import qualified Data.Map as M

import Printcess.Core
import Printcess.Config -- For haddock links to work...

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
--   > pretty defConfig $ "," `betweenEach` []          -- ↪ ""
--   > pretty defConfig $ "," `betweenEach` ["x"]       -- ↪ "x"
--   > pretty defConfig $ "," `betweenEach` ["x", "y"]  -- ↪ "x,y"
infixl 6 `betweenEach`
betweenEach :: (Pretty a, Pretty b) => a → [b] → PrettyM ()
betweenEach s as = sepByA_ (map pp as) (pp s)

-- | Print an @a@ before each @b@.
--
--   Examples:
--
--   > pretty defConfig $ "," `beforeEach` []          -- ↪ ""
--   > pretty defConfig $ "," `beforeEach` ["x"]       -- ↪ ",x"
--   > pretty defConfig $ "," `beforeEach` ["x", "y"]  -- ↪ ",x,y"
infixl 6 `beforeEach`
beforeEach :: (Pretty a, Pretty b) => a → [b] → PrettyM ()
beforeEach a bs = foldl (>>) (pure ()) $ map pp bs `sepByL'` pp a

-- | Print an @a@ after each @b@.
--
--   Examples:
--
--   > pretty defConfig $ "," `afterEach` []          -- ↪ ""
--   > pretty defConfig $ "," `afterEach` ["x"]       -- ↪ "x,"
--   > pretty defConfig $ "," `afterEach` ["x", "y"]  -- ↪ "x,y,"
infixl 6 `afterEach`
afterEach :: (Pretty a, Pretty b) => a → [b] → PrettyM ()
afterEach a bs = foldl (>>) (pure ()) $ map pp bs `sepByR'` pp a

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
block  xs = indented $ nl `beforeEach` xs

-- | Same as 'block', but starts the block on the current line.
--
-- Example:
--
-- > pretty defConfig $ "do" ~> block' ["putStrLn hello", "putStrLn world"]
-- > -- ↪ "do putStrLn hello
-- > --       putStrLn world"
block' :: Pretty a => [a] → PrettyM ()
block' xs = indentedToCurPos $ nl `betweenEach` xs

-- | Print a @[a]@ similar to its 'Show' instance.
--
--   Example:
--
--   > pretty defConfig $ ppList [ "x", "y" ]  -- ↪ "[ x, y ]"
ppList :: Pretty a => [a] → PrettyM ()
ppList ps = "[" ~> ", " `betweenEach` ps ~> "]"

-- | Print a list map @[(k,v)]@ as 'ppList', but render @(k,v)@ pairs as @"k → v"@.
--
--   Example:
--
--   > pretty defConfig $ ppListMap [ ("k1", "v1"), ("k2", "v2") ]
--   > -- ↪ "[ k1 → v1, k2 → v2 ]"
ppListMap :: (Pretty a, Pretty b) => [(a, b)] → PrettyM ()
ppListMap = ppList . map (\(a,b) → a ~> "→" ~> b)

-- | Print a @Data.Map@ in the same way as 'ppListMap'.
ppMap :: (Pretty a, Pretty b) => M.Map a b → PrettyM ()
ppMap = ppListMap . M.assocs

-- | Print a horizontal bar consisting of a 'Char' as long as 'cMaxLineWidth'
--   (or 80 if it is @Nothing@).
--
--   Example:
--
--   > pretty defConfig $ bar '-'
--   > -- ↪ "-----------------------------------------…"
bar :: Char → PrettyM ()
bar c = do
  w ← maybe 80 id <$> use maxLineWidth
  pp $ replicate w c

-- | Print a horizontal bar consisting of a 'Char' as long as 'cMaxLineWidth'
--   (or 80 if it is @Nothing@). The horizontal bar has a title 'String' printed
--   at column 6.
--
--   Example:
--
--   > pretty defConfig $ titleBar '-' "Foo"
--   > -- ↪ "----- Foo -------------------------------…"
titleBar :: Pretty a => Char → a → PrettyM ()
titleBar c s = do
  w ← maybe 80 id <$> use maxLineWidth
  replicate 5 c ~> s ~> replicate (w - (7 + length (pretty (pure ()) s))) c +> "\n"

-- | Print a newline (line break).
nl :: PrettyM ()
nl = pp "\n"

-- | Print a space.
sp :: PrettyM ()
sp = pp " "

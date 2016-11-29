{-# LANGUAGE
  TemplateHaskell, UnicodeSyntax
#-}

module Printcess.Config where

import Control.Monad.State.Lazy
import Control.Lens

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
-- >   where config :: State Config ()
-- >         config = do cMaxLineWidth      .= Just 6
-- >                     cInitIndent        .= 2
-- >                     cIndentAfterBreaks .= 0
data Config = Config
  { _configMaxLineWidth      :: Maybe Int
  , _configInitPrecedence    :: Int
  , _configInitIndent        :: Int
  , _configIndentChar        :: Char
  , _configIndentDepth       :: Int
  , _configIndentAfterBreaks :: Int
  }

def :: Config
def = Config
  { _configMaxLineWidth      = Just 80
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
--   Default: @Just 80@
cMaxLineWidth :: Lens' Config (Maybe Int)
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
--   > pretty (do cMaxLineWidth .= Just 8; cIndentAfterBreaks .= 4) "foo bar baz boo"
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

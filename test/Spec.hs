{-# lANGUAGE LambdaCase #-}

import Test.Hspec
import Printcess.PrettyPrinting
import Control.Lens

main :: IO ()
main = hspec $ do
  describe "basic" $ do
    it "prints a string" $
      pretty defConfig "foo" `shouldBe` "foo"
  describe "automatic line breaks" $ do
    it "breaks too long lines" $
      pretty (cMaxLineWidth .= Just 10) "foo bar baz boo"
        `shouldBe` "foo bar \n    baz \n    boo"
    it "breaks too long lines" $
      pretty (cMaxLineWidth .= Just 11) "foo bar baz boo"
        `shouldBe` "foo bar baz\n    boo"
    it "breaks too long lines twice" $
      pretty (cMaxLineWidth .= Just 10) "foo bar baz boo r"
        `shouldBe` "foo bar \n    baz \n    boo r"
    it "breaks too long lines four times" $
      pretty (cMaxLineWidth .= Just 10) "foo bar baz boo raz roo"
        `shouldBe` "foo bar \n    baz \n    boo \n    raz \n    roo"
    it "breaks too long lines" $
      pretty (cMaxLineWidth .= Just 11) "foo bar baz boo r"
        `shouldBe` "foo bar baz\n    boo r"
    it "breaks too long lines twice" $
      pretty (cMaxLineWidth .= Just 11) "foo bar baz boo raz roo"
        `shouldBe` "foo bar baz\n    boo raz\n    roo"
    it "breaks too long lines" $
      pretty (cMaxLineWidth .= Just 12) "foo bar baz boo r"
        `shouldBe` "foo bar baz \n    boo r"
    it "breaks too long lines & setting indentChar works" $
      pretty (do cMaxLineWidth .= Just 10; cIndentChar .= '~') "foo bar baz boo r"
        `shouldBe` "foo bar \n~~~~baz \n~~~~boo r"
  describe "Lambda Calculus" $ do
    it "respects associativity" $
      pretty defConfig e1
        `shouldBe` "λx. λy. x y (x y)"
  describe "Blocks" $ do
    it "blocks starting on next line" $
      pretty defConfig ("do" ++> block [ "ma", "mb" ])
        `shouldBe` "do \n  ma\n  mb"
    it "blocks starting on same line" $
      pretty defConfig ("do" ++> block' [ "ma", "mb" ])
        `shouldBe` "do ma\n   mb"

data Expr
  = EVar String
  | EAbs String Expr
  | EApp Expr Expr

instance Pretty Expr where
  pp = \case
    EVar x     -> pp x
    EAbs x e   -> assocR 0 $ "λ" +> x +> "." ++> R e
    EApp e1 e2 -> assocL 9 $ L e1 ++> R e2

e1 = EAbs "x" $ EAbs "y" $ EApp (EApp (EVar "x") (EVar "y"))
                                (EApp (EVar "x") (EVar "y"))

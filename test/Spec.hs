{-# lANGUAGE LambdaCase #-}

import Test.Hspec
import Printcess.PrettyPrinting
import Control.Lens

main :: IO ()
main = hspec $ do
  describe "basic" $ do
    it "prints a string" $
      pretty def "foo" `shouldBe` "foo"
  describe "automatic line breaks" $ do
    it "breaks too long lines" $
      pretty (def & configMaxLineWidth .~ 10) "foo bar baz boo"
        `shouldBe` "foo bar \n    baz \n    boo"
    it "breaks too long lines" $
      pretty (def & configMaxLineWidth .~ 11) "foo bar baz boo"
        `shouldBe` "foo bar baz\n    boo"
    it "breaks too long lines twice" $
      pretty (def & configMaxLineWidth .~ 10) "foo bar baz boo r"
        `shouldBe` "foo bar \n    baz \n    boo r"
    it "breaks too long lines four times" $
      pretty (def & configMaxLineWidth .~ 10) "foo bar baz boo raz roo"
        `shouldBe` "foo bar \n    baz \n    boo \n    raz \n    roo"
    it "breaks too long lines" $
      pretty (def & configMaxLineWidth .~ 11) "foo bar baz boo r"
        `shouldBe` "foo bar baz\n    boo r"
    it "breaks too long lines twice" $
      pretty (def & configMaxLineWidth .~ 11) "foo bar baz boo raz roo"
        `shouldBe` "foo bar baz\n    boo raz\n    roo"
    it "breaks too long lines" $
      pretty (def & configMaxLineWidth .~ 12) "foo bar baz boo r"
        `shouldBe` "foo bar baz \n    boo r"
    it "breaks too long lines & setting indentChar works" $
      pretty (def & configMaxLineWidth .~ 10 & configIndentChar .~ '~') "foo bar baz boo r"
        `shouldBe` "foo bar \n~~~~baz \n~~~~boo r"
  describe "Lambda Calculus" $ do
    it "respects associativity" $
      pretty def e1
        `shouldBe` "λx. λy. x y (x y)"

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

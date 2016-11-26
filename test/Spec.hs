import Test.Hspec
import Printcess.PrettyPrinting
import Control.Lens

main :: IO ()
main = hspec $ do
  describe "printcess" $ do
    it "prints a string" $
      pretty def "foo" `shouldBe` "foo"
    it "breaks too long lines" $
      pretty (def & configMaxLineWidth .~ 10) "foo bar baz boo"
        `shouldBe` "foo bar \n    baz \n    boo"
    it "breaks too long lines" $
      pretty (def & configMaxLineWidth .~ 11) "foo bar baz boo"
        `shouldBe` "foo bar baz\n    boo"
    it "breaks too long lines twice" $
      pretty (def & configMaxLineWidth .~ 10) "foo bar baz boo r"
        `shouldBe` "foo bar \n    baz \n    boo r"
    it "breaks too long lines twice" $
      pretty (def & configMaxLineWidth .~ 10) "foo bar baz boo raz roo"
        `shouldBe` "foo bar \n    baz \n    boo \n    raz \n    roo"
    it "breaks too long lines twice" $
      pretty (def & configMaxLineWidth .~ 11) "foo bar baz boo r"
        `shouldBe` "foo bar baz\n    boo r"
    it "breaks too long lines twice" $
      pretty (def & configMaxLineWidth .~ 11) "foo bar baz boo raz roo"
        `shouldBe` "foo bar baz\n    boo raz\n    roo"
    it "breaks too long lines twice" $
      pretty (def & configMaxLineWidth .~ 12) "foo bar baz boo r"
        `shouldBe` "foo bar baz \n    boo r"

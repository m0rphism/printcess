import Test.Hspec
import Printcess.PrettyPrinting
import Control.Lens

main :: IO ()
main = hspec $ do
  describe "printcess" $ do
    it "prints a string" $
      pretty def "foo" `shouldBe` "foo"
    it "breaks too long lines" $
      pretty (def & configMaxLineWidth .~ 11) "foo bar baz boo"
        `shouldBe` "foo bar \n    baz boo"
    -- it "breaks too long lines twice" $
    --   pretty (def & configMaxLineWidth .~ 11) "foo bar baz boo r"
    --     `shouldBe` "foo bar \n    baz boo \n    r"
    -- it "breaks too long lines twice" $
    --   pretty (def & configMaxLineWidth .~ 11) "foo bar baz boo raz roo"
    --     `shouldBe` "foo bar \n    baz boo \n    raz roo"

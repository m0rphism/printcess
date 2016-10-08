import Test.Hspec
import Printcess.PrettyPrinting

main :: IO ()
main = hspec $ do
  describe "printcess" $ do
    it "prints a string" $
      pretty def "foo" `shouldBe` "foo"

import ArithSpec
import Test.Hspec
import UntypedSpec

main :: IO ()
main =
  hspec $ do
    ArithSpec.spec
    UntypedSpec.spec

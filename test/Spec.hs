import ArithSpec
import SimplyTypedSpec
import Test.Hspec
import UntypedSpec

main :: IO ()
main =
  hspec $ do
    ArithSpec.spec
    UntypedSpec.spec
    SimplyTypedSpec.spec

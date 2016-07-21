
import           Test.Hspec

import qualified ClientTests
import qualified ServerTests

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ServerTests.spec
  ClientTests.spec

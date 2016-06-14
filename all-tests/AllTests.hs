
import           System.Directory
import           Test.Hspec

import qualified ClientTests
import qualified ServerTests

main :: IO ()
main = do
  setCurrentDirectory ".."
  hspec spec

spec :: Spec
spec = do
    ServerTests.spec
    ClientTests.spec

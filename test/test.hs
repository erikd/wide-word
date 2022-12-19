import           Control.Monad (unless)

import           System.Exit (exitFailure)

import qualified Test.Data.WideWord.Int128
import qualified Test.Data.WideWord.Word64
import qualified Test.Data.WideWord.Word128

main :: IO ()
main = runTests
  [ Test.Data.WideWord.Int128.tests
  , Test.Data.WideWord.Word64.tests
  , Test.Data.WideWord.Word128.tests
  ]

runTests :: [IO Bool] -> IO ()
runTests tests = do
  result <- and <$> sequence tests
  unless result
    exitFailure

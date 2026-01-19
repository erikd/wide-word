import           Control.Monad (unless)

import           System.Exit (exitFailure)

import qualified Test.Data.WideWord.Word256
import qualified Test.Data.WideWord.Int256

main :: IO ()
main = runTests
  [ Test.Data.WideWord.Word256.tests
  , Test.Data.WideWord.Int256.tests
  ]

runTests :: [IO Bool] -> IO ()
runTests tests = do
  result <- and <$> sequence tests
  unless result
    exitFailure

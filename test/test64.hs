import           Control.Monad (unless)

import           System.Exit (exitFailure)

import qualified Test.Data.WideWord.Word64

main :: IO ()
main = runTests
  [ Test.Data.WideWord.Word64.tests
  ]

runTests :: [IO Bool] -> IO ()
runTests tests = do
  result <- and <$> sequence tests
  unless result
    exitFailure

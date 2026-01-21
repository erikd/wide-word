{-# LANGUAGE ImportQualifiedPost #-}
import Control.Monad (unless)

import System.Exit (exitFailure)

import Test.Data.WideWord.Int128 qualified
import Test.Data.WideWord.Word128 qualified

main :: IO ()
main = runTests
  [ Test.Data.WideWord.Int128.tests
  , Test.Data.WideWord.Word128.tests
  ]

runTests :: [IO Bool] -> IO ()
runTests tests = do
  result <- and <$> sequence tests
  unless result
    exitFailure

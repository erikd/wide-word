import Control.Monad (when)

import Test.Data.WideWord.Int128
import Test.Data.WideWord.Word128

import Test.Hspec (Spec)
import Test.Hspec.Runner (configQuickCheckMaxSuccess, defaultConfig, hspecWithResult, summaryFailures)

import System.Exit (exitFailure, exitSuccess)



main :: IO ()
main = do
  summary <- hspecWithResult config testAll
  when (summaryFailures summary == 0)
    exitSuccess
  exitFailure
  where
    config = defaultConfig { configQuickCheckMaxSuccess = Just 100000 }

testAll :: Spec
testAll = do
  testWord128
  testInt128


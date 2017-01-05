import Control.Monad (when)

import Test.Data.WideWord.Word128

import Test.Hspec (Spec)
import Test.Hspec.Runner (defaultConfig, hspecWithResult, summaryFailures)

import System.Exit (exitFailure, exitSuccess)



main :: IO ()
main = do
  summary <- hspecWithResult defaultConfig testAll
  when (summaryFailures summary == 0)
    exitSuccess
  exitFailure


testAll :: Spec
testAll =
  testWord128


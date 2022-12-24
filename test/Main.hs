module Main (main) where

import Lyric (expAlts, expApp, multiStep, runM)
import Lyric.Core (Err, Exp (..), Op (..), RetVal (..), St, Val (..), initSt)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

evalExp :: Exp -> Either (Err, St) RetVal
evalExp ex =
  let (erv, st) = runM multiStep (initSt ex)
  in case erv of
    Left er -> Left (er, st)
    Right rv -> Right rv

testCases :: TestTree
testCases = testCase "cases" $ do
  -- Basic primitive op
  let caseAdd = expApp (ExpOp OpAdd) [ExpInt 1, ExpInt 2]
  evalExp caseAdd @?= Right (RetValPure (ValInt 3))

  -- Basic alternative
  let caseAlt = expAlts [ExpFail, ExpInt 1]
  evalExp caseAlt @?= Right (RetValPure (ValInt 1))

main :: IO ()
main = defaultMain (testGroup "Lyric" [testCases])

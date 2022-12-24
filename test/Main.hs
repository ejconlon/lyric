module Main (main) where

import qualified Data.Sequence as Seq
import Lyric (expAlts, expApp, expTup, multiStep, runM)
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
  -- Primitive op
  let caseAdd = expApp (ExpOp OpAdd) [ExpInt 1, ExpInt 2]
  evalExp caseAdd @?= Right (RetValPure (ValInt 3))

  -- Alternative (succeeding)
  let caseAlt = expAlts [ExpFail, ExpInt 1]
  evalExp caseAlt @?= Right (RetValPure (ValInt 1))

  -- Tuple
  let caseTup = expTup [ExpInt 1, ExpInt 2]
  evalExp caseTup @?= Right (RetValPure (ValTup (Seq.fromList [ValInt 1, ValInt 2])))

main :: IO ()
main = defaultMain (testGroup "Lyric" [testCases])

{-# LANGUAGE OverloadedStrings #-}

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
  let valOne = ValInt 1
  let valTup = ValTup (Seq.fromList [ValInt 1, ValInt 2])

  -- Primitive op
  let caseAdd = expApp (ExpOp OpAdd) [ExpInt 1, ExpInt 2]
  evalExp caseAdd @?= Right (RetValPure (ValInt 3))

  -- Alternative (succeeding) - implicit one
  let caseAlt = expAlts [ExpFail, ExpInt 1, ExpInt 2]
  evalExp caseAlt @?= Right (RetValPure valOne)

  -- Tuple
  let caseTup = expTup [ExpInt 1, ExpInt 2]
  evalExp caseTup @?= Right (RetValPure valTup)

  -- Fail
  let caseFail = ExpFail
  evalExp caseFail @?= Right RetValFail

  -- Alternative (succeeding) - explicit one
  let caseOne = ExpOne caseAlt
  evalExp caseOne @?= Right (RetValPure valOne)

  -- All alternatives
  let caseAll = ExpAll caseAlt
  evalExp caseAll @?= Right (RetValPure valTup)

  -- Exists var
  let caseExistsVar = ExpExists "x" (ExpVar "x")
  case evalExp caseExistsVar of
    Right (RetValPure (ValVar _)) -> pure ()
    other -> fail ("exists var failed: " ++ show other)

main :: IO ()
main = defaultMain (testGroup "Lyric" [testCases])

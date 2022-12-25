{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Lyric (multiStep, runM)
import Lyric.Core (Err, Exp (..), Op (..), RetVal (..), St, Val (..), expAlts, expApp, expLam, expTup, initSt)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

evalExp :: Exp -> Either (Err, St) RetVal
evalExp ex =
  let (erv, st) = runM multiStep (initSt ex)
  in case erv of
    Left er -> Left (er, st)
    Right rv -> Right rv

testCases :: TestTree
testCases =
  let valOne = ValInt 1
      valTup = ValTup (Seq.fromList [ValInt 1, ValInt 2])
      caseAlt = expAlts [ExpFail, ExpInt 1, ExpInt 2]
  in testGroup "cases"
    [ testCase "val op" $ do
      let e = ExpOp OpAdd
      evalExp e @?= Right (RetValPure (ValOp OpAdd Empty))
    , testCase "val int" $ do
      let e = ExpInt 1
      evalExp e @?= Right (RetValPure valOne)
    , testCase "val lam" $ do
      let e = expLam "x" (ExpVar "x")
      evalExp e @?= Right (RetValPure (ValLam Map.empty "x" (ExpVar "x")))
    , testCase "red op" $ do
      let e = expApp (ExpOp OpAdd) [ExpInt 1, ExpInt 2]
      evalExp e @?= Right (RetValPure (ValInt 3))
    , testCase "alt implicit" $ do
      let e = caseAlt
      evalExp e @?= Right (RetValPure valOne)
    , testCase "tuple" $ do
      let e = expTup [ExpInt 1, ExpInt 2]
      evalExp e @?= Right (RetValPure valTup)
    , testCase "fail" $ do
      let e = ExpFail
      evalExp e @?= Right RetValFail
    , testCase "alt explicit" $ do
      let e = ExpOne caseAlt
      evalExp e @?= Right (RetValPure valOne)
    , testCase "all" $ do
      let e = ExpAll caseAlt
      evalExp e @?= Right (RetValPure valTup)
    , testCase "exists var" $ do
      let e = ExpExists "x" (ExpVar "x")
      evalExp e @?= Right (RetValPure (ValVar 0))
    , testCase "exists lam free" $ do
      let e = ExpExists "y" (expLam "x" (ExpVar "y"))
      evalExp e @?= Right (RetValPure (ValLam (Map.singleton "y" 0) "x" (ExpVar "y")))
    -- , testCase "lam app" $ do
    --   let e = expApp (expLam "x" (ExpVar "x")) [ExpInt 1]
    --   evalExp e @?= Right (RetValPure valOne)
    ]

main :: IO ()
main = defaultMain (testGroup "Lyric" [testCases])

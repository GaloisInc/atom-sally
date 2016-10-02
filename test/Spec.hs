{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Language.Atom hiding (compile)

import Language.Sally.Types
import Language.Sally.Translation
import Language.Sally.PPrint


-- Test Atoms -----------------------------------------------------------

-- | 'x' starts at 0, increases each tick up to 10
atom1 :: Atom ()
atom1 = atom "atom1" $ do
  x <- int8  "x" 0
  cond $ (value x) <. 10
  x <== (value x) + 1

basicAtomTests :: TestTree
basicAtomTests = testGroup "basic Atom translation tests"
  [ testCompile "atom1" atom1 ]

testCompile :: String -> Atom () -> TestTree
testCompile nm atom' = testCase ("testCompile " ++ nm) $ do
  tr <- compile (nameFromS nm) TrConfig atom'
  putSystemLn tr
  assertBool "" True


-- Main -----------------------------------------------------------------

suite :: TestTree
suite = testGroup "atom-sally Test Suite"
          [ basicAtomTests
          ]

main :: IO ()
main = defaultMain suite

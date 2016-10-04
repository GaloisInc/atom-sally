{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.FilePath.Posix
import System.IO

import Language.Atom hiding (compile)

import Language.Sally.Types
import Language.Sally.Translation
import Language.Sally.PPrint

testDir :: FilePath
testDir = "test"


-- Test Atoms -----------------------------------------------------------

-- | 'x' starts at 0, increases each tick up to 10
atom1 :: Atom ()
atom1 = atom "atom1" $ do
  x <- int8  "x" 0
  cond $ (value x) <. 10
  x <== (value x) + 1

atom2 :: Atom()
atom2 = atom "atom2" $ do
  f <- bool "flag" False

  atom "alice" $ do
    a <- bool "a" False
    cond $ (value f)
    a <== Const True

  atom "bob" $ do
    f <== Const True


-- Main -----------------------------------------------------------------

putHeader = putStrLn (replicate 72 '-')

testCompile :: (String, Atom (), String) -> IO ()
testCompile (nm, atom', q) = do
  tr <- compile (nameFromS nm) TrConfig atom'
  let fname = testDir </> nm ++ ".mcmt"
  withFile fname WriteMode $ \h -> do
    hPutSystem tr h
    hPutStrLn h "\n;; Query"
    hPutStrLn h q
  putStrLn ("compiled " ++ fname)

-- | List of (Name, Atom, Query) to translate and print
suite :: [(String, Atom (), String)]
suite =
  [ ("A1", atom1,
        "(query A1_transition_system (<= 0 A1!atom1!x))")
  , ("A2", atom2,
        "(query A2_transition_system (=> A2!atom2!alice!a A2!atom2!flag))")
  ]

main :: IO ()
main = do
  mapM_ testCompile suite

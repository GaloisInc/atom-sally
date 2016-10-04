{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int
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
--   Property: G(atom1!x >= 0)
atom1 :: Atom ()
atom1 = atom "atom1" $ do
  x <- int8  "x" 0
  cond $ (value x) <. 10
  x <== (value x) + 1

-- | Two Atoms communicate through a flag
--   Property: G(atom2!alice!a => atom2!flag)
atom2 :: Atom()
atom2 = atom "atom2" $ do
  f <- bool "flag" False

  atom "alice" $ do
    a <- bool "a" False
    cond $ (value f)
    a <== Const True

  atom "bob" $ do
    f <== Const True

type MsgType = Int64

-- | Two Atoms communicate through a *channel*
--   Property: G((/= atom3!bob!msg -1) => atom3!alice!done)
--             F((/= atom3!bob!msg -1))
atom3 :: Atom()
atom3 = atom "atom3" $ do

  let
    -- | Special message value indicating "no message present"
    missingMsgValue :: MsgType
    missingMsgValue = (-1)

    -- | Special message value indicating "correct (intended) message"
    goodMsg :: E MsgType
    goodMsg = Const 1

  (cin, cout) <- channel "aTob" missingMsgValue

  atom "alice" $ do
    done <- bool "done" False
    writeChannel cin goodMsg
    done <== Const True

  atom "bob" $ do
    msg <- int64 "msg" missingMsgValue
    condChannel cout
    msg <== readChannel cout


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
  , ("A3", atom3,
        unwords [ "(query A3_transition_system"
                , "  (=> (not (= A3!atom3!bob!msg (-1))) A3!atom3!alice!done))"])
  ]

main :: IO ()
main = do
  mapM_ testCompile suite

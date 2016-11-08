{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int
import System.FilePath.Posix
import System.IO

import Language.Atom hiding (compile)
import Language.Sally

testDir :: FilePath
testDir = "test"

type MsgType = Int64
msgType = Int64  -- Atom 'Type' value

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


-- | Two Atoms communicate through a *channel*
--   Property: G((/= atom3!bob!msg -1) => atom3!alice!done)
--             F((/= atom3!bob!msg -1))
atom3 :: Atom()
atom3 = atom "atom3" $ do

  let
    -- | Special message values indicating "no message present", and "correct
    -- (intended) message"
    missingMsgValue, goodMsgValue  :: MsgType
    missingMsgValue = -1
    goodMsgValue    = 1

  (cin, cout) <- channel "aTob" msgType

  atom "alice" $ do
    done <- bool "done" False
    writeChannel cin (Const goodMsgValue)
    done <== Const True

  atom "bob" $ do
    msg <- int64 "msg" missingMsgValue
    cond $ fullChannel cout
    msg <== readChannel cout



-- Main -----------------------------------------------------------------

putHeader = putStrLn (replicate 72 '-')

testCompile :: (String, Atom (), String) -> IO ()
testCompile (nm, spec, q) = do
  let fname = testDir </> nm ++ ".mcmt"
  let cfg = defaultCfg { cfgTopNameSpace = False }
  compileToSally nm cfg fname spec (Just q)
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

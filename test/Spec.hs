{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int
import qualified Data.Map.Strict as Map
import           Control.Monad (void)
import System.FilePath.Posix
import System.IO

import           Language.Atom hiding (compile)
import qualified Language.Atom as A
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


-- | Three Atoms communicate through two channels
--
--   Property: node C is done implies that node C's 'msg' variable equals 1
--   ('goodMsgValue'). Futhermore, node C is done implies that the global time
--   is equal to 2.
--
--   (=> A4!atom4!nodeC!done (and (= A4!atom4!nodeC!msg 1)
--                                (= A4!__t 2)))
atom4 :: Atom()
atom4 = atom "atom4" $ do

  let
    -- | Special message values indicating "no message present", and "correct
    -- (intended) message"
    missingMsgValue, goodMsgValue  :: MsgType
    missingMsgValue = -1
    goodMsgValue    = 1

  (cinA2B, coutA2B) <- channel "a2b" msgType
  (cinB2C, coutB2C) <- channel "b2c" msgType

  atom "nodeA" $ do
    done <- bool "done" False
    writeChannel cinA2B (Const goodMsgValue)
    done <== Const True

  atom "nodeB" $ do
    done <- bool "done" False
    msg <- int64 "msg" missingMsgValue
    cond (fullChannel coutA2B)
    writeChannel cinB2C (readChannel coutA2B :: E MsgType)
    msg <== readChannel coutA2B
    done <== Const True

  atom "nodeC" $ do
    done <- bool "done" False
    msg <- int64 "msg" missingMsgValue
    cond (fullChannel coutB2C)
    msg <== readChannel coutB2C
    done <== Const True

-- | Atom setting and using a timer based on the time at which it
--   received a message.

atom5 :: Atom()
atom5 = atom "atom5" $ do

  let
    -- | Special message values indicating "no message present", and "correct
    -- (intended) message"
    missingMsgValue, goodMsgValue  :: MsgType
    missingMsgValue = -1
    goodMsgValue    = 1

  (cin, cout) <- channel "chan" msgType
  rxTime <- word64 "rxTime" 0

  atom "alice" $ do
    done <- bool "done" False
    writeChannel cin (Const goodMsgValue)
    done <== Const True

  atom "bob" $ do

    atom "recMsg"  $ do  
      msg <- int64 "msg" missingMsgValue
      cond $ fullChannel cout
      msg <== readChannel cout
      rxTime <== clock

    atom "timerDone" $ do
      local <- bool "local" False
      cond (value rxTime + 1000 <. clock)
      local <== Const True
    
compileAtom5 :: IO ()
compileAtom5 =
  void $ A.compile "atom5" defaults atom5
  

-- Configurations --------------------------------------------------------------

-- | Default config for these specs
defSpecCfg :: TrConfig
defSpecCfg = defaultCfg { cfgDebug = True }

-- | Example of a hybrid fault model configuration.
hybridCfg :: TrConfig
hybridCfg = defSpecCfg { cfgMFA = HybridFaults ws 0 }
  where ws = Map.fromList [ (NonFaulty, 0), (ManifestFaulty, 1), (SymmetricFaulty, 2)
                          , (ByzantineFaulty, 3)
                          ]

-- | Example of a fixed fault mapping (specific to 'atom2' above).
fixedCfg :: TrConfig
fixedCfg = defSpecCfg { cfgMFA = FixedFaults mp }
  where mp = Map.fromList [ ("A2b!atom2!alice", NonFaulty)
                          , ("A2b!atom2!bob",   ByzantineFaulty)
                          ]

-- Main -----------------------------------------------------------------

putHeader = putStrLn (replicate 72 '-')

testCompile :: (String, Atom (), TrConfig, String) -> IO ()
testCompile (nm, spec, cfg, q) = do
  let fname = testDir </> nm ++ ".mcmt"
  compileToSally nm cfg fname spec (Just q)
  putStrLn ("compiled " ++ fname)

-- | List of (Name, Atom, Query) to translate and print
suite :: [(String, Atom (), TrConfig, String)]
suite =
  [ ("A1", atom1, hybridCfg,
        "(query A1_transition_system (=> A1_mfa_formula (<= 0 A1!atom1!x)))")
  , ("A1b", atom1, defSpecCfg,
        "(query A1b_transition_system (=> A1b_mfa_formula (<= 0 A1b!atom1!x)))")
  , ("A2", atom2, hybridCfg,
        "(query A2_transition_system (=> A2_mfa_formula (=> A2!atom2!alice!a A2!atom2!flag)))")
  , ("A2b", atom2, fixedCfg,
        "(query A2b_transition_system (=> A2b_mfa_formula (=> A2b!atom2!alice!a A2b!atom2!flag)))")
  , ("A3", atom3, hybridCfg,
        unwords [ "(query A3_transition_system"
                , "  (=> A3_mfa_formula"
                , "    (=> (not (= A3!atom3!bob!msg (-1))) A3!atom3!alice!done)))"])
  , ("A4", atom4, defSpecCfg,
        unwords [ "(query A4_transition_system"
                , "  (=> A4_mfa_formula"
                , "    (=> A4!atom4!nodeC!done (= A4!atom4!nodeC!msg 1))))"
                , "\n\n"
                , "(query A4_transition_system"
                , "  (=> A4_mfa_formula"
                , "    (=> A4!atom4!nodeC!done (= A4!__t 2))))"])
--  , ("A5", atom5, defSpecCfg, "")
  ]

main :: IO ()
main = do
  mapM_ testCompile suite
  compileAtom5

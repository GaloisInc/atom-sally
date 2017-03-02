{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Int
import qualified Data.Map.Strict as Map
import           Control.Monad (void)
import System.FilePath.Posix
import           Language.Atom hiding (compile)
import qualified Language.Atom as A
import Language.Sally

testDir :: FilePath
testDir = "test"

type MsgType = Int64

msgType :: Type
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
    consumeChannel cout

-- | A minimal version of atom3, where two agents commicate one message over a
-- channel. This version has no "done" flags.
--   Property: G((/= atom3!bob!msg -1) => (= atom3!__t 1))
--             F((= atom3!__t 1))
atom3min :: Atom()
atom3min = atom "atom3" $ do

  let
    -- | Special message values indicating "no message present", and "correct
    -- (intended) message"
    missingMsgValue, goodMsgValue  :: MsgType
    missingMsgValue = -1
    goodMsgValue    = 1

  (cin, cout) <- channel "aTob" msgType

  atom "alice" $ do
    writeChannel cin (Const goodMsgValue)

  atom "bob" $ do
    msg <- int64 "msg" missingMsgValue
    cond $ fullChannel cout
    msg <== readChannel cout
    consumeChannel cout

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
    consumeChannel coutA2B

  atom "nodeC" $ do
    done <- bool "done" False
    msg <- int64 "msg" missingMsgValue
    cond (fullChannel coutB2C)
    msg <== readChannel coutB2C
    done <== Const True
    consumeChannel coutB2C

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

  atom "alice" $ do
    done <- bool "done" False
    writeChannel cin (Const goodMsgValue)
    probe "alice.done" (value done)
  
    done <== Const True

  atom "bob" $ do

    rxTime <- word64 "rxTime" 0

    atom "recMsg"  $ do  
      msg <- int64 "msg" missingMsgValue
      cond $ fullChannel cout
      msg <== readChannel cout
  
      probe "bob.recMsg" (value msg)
      rxTime <== clock

    atom "timerDone" $ do
      local <- bool "local" False
      cond (value rxTime + 1000 >. clock)
      local <== Const True

    printAllProbes

 

buttonPeriod :: Int
buttonPeriod = 10

observerPeriod :: Int
observerPeriod = 1

procPeriod :: Int
procPeriod = 2


atom_wbs :: Atom()
atom_wbs = atom "atom_wbs" $ do
 --let zeroC  = Const zero
 -- let twoC   = Const two
 let threeC = Const three
   
   -- button press to com and mon
 (bpinc, bpComout) <- channel "bpcout" Bool
 (bpinm, bpMonout) <- channel "bpmout" Bool

 -- com to mon and mon to com state echange
 -- transfers state of 
-- (mtocin, mtocout) <- channel "montocom" Bool
 (ctomin, ctomout) <- channel "comtomon" Bool


 (bcin, bcout) <- channel "chanbo" Int64
 (ccin, ccout) <- channel "chanco" Int64
 (mcin, mcout) <- channel "chanmo" Int64

-- (cin, cout) <- channel "button_chan" msgType
 period buttonPeriod . atom "button" $ do
    count <- var "count" zero    -- declare a local variable
    bs <- bool "bs" False

   -- autoMode <- bool "autoMode" False
    probe "boolbutton.count" (value count)
    bs <== mux (value bs ==. Const True) (Const False) (Const True)
    writeChannel bpinc (value bs) -- channel for command button input
    writeChannel bpinm (value bs) -- channel for monitor button input
    writeChannel bcin (value count)  -- put 'count' on the channel for observer
    incr count 

                       -- increment count (order w/ writeChannel
                                -- doesn't matter
 period procPeriod . atom "command" $ do
    framecount <- var "framecount" zero 
    bs <- var  "bs"  False
    prevbs <- var  "prevbs"  False
    cautoMode <- var "cautoMode" False
      -- declare a local variable
    prevbs <== value bs
    writeChannel ccin (value framecount)  -- put 'count' on the channel
    incr framecount 
    writeChannel ctomin (value cautoMode)
    probe "command.autoMode" (value cautoMode)
    

    atom "wait_for_button_press" $ do
      cond $ fullChannel bpComout
      bs <== readChannel bpComout
      probe "command.button_pressed" (value bs)
      cautoMode <== mux ((value bs ==. Const True) &&. (value prevbs /=. Const False))
                       (not_ (value cautoMode))
                       (value cautoMode)
      consumeChannel bpComout

      
 
 period procPeriod . atom "monitor" $ do
    framecount <- var "count" zero    -- declare a local variable
    bs <- var  "bs"  False
    prevbs <- var "prevbs" False
    mautoMode <- var "mautoMode" False
    xSideAutoMode <- var "autoMode" False
    agreementFailureCount <- var "agreementFailureCount" zero
    agreementFailure <- var "agreementFailure" False
    
    prevbs <== value bs
    writeChannel mcin (value framecount)  -- put 'count' on the channel
    incr framecount 
  --  writeChannel mtocin (value autoMode)
    probe "monitor.autoMode" (value mautoMode)
    mautoMode <== mux ((value bs ==. Const True) &&. (value prevbs /=. Const False))
                       (not_ (value mautoMode))
                       (value mautoMode)

    atom "wait_for_button_press" $ do
      cond $ fullChannel bpMonout
      bs <== readChannel bpMonout
      probe "monitor.button_pressed" (value bs)
      consumeChannel bpMonout
    
    atom "wait_x_side_autoMode" $ do
      cond $ fullChannel ctomout
      xSideAutoMode <== readChannel ctomout
      consumeChannel ctomout
      probe "monitor.XsideAutoMode=" (value xSideAutoMode)
      
    atom "mon_agreement" $ do
      cond $ value mautoMode /=. value xSideAutoMode
      incr agreementFailureCount

    atom "mon_agreement_count" $ do 
      cond $ value agreementFailureCount ==. threeC
      agreementFailure <== Const True


                       -- increment count (order w/ writeChannel
 
 period observerPeriod . atom "observer" $ do
    bcount <- var "bcount" zero 
    ccount <- var "ccount" zero 
    mcount <- var "mcount" zero 
    probe "observer.bcount" (value bcount)
    probe "observer.com_framecount" (value ccount)
    probe "observer.mon_framecount" (value mcount)
 
    atom "wait_for_button_frame" $ do
     cond $ fullChannel bcout
     bcount <== readChannel bcout
     consumeChannel bcout
    
    atom "wait_for_com_frame" $ do
     cond $ fullChannel ccout
     ccount <== readChannel ccout
     consumeChannel ccout
    
    atom "wait_for_mon_frame" $ do
     cond $ fullChannel mcout
     mcount <== readChannel mcout
     consumeChannel mcout
    

 printAllProbes


compileAtom5 :: IO ()
compileAtom5 = void $ A.compile ("atom5") defaults atom5
  
compileAtomWBS :: IO ()
compileAtomWBS = void $ A.compile ("atom_wbs") defaults atom_wbs

-- Utility Stuff -------------------------------------------------------

zero, two, three :: Int64
zero   = 0
two    = 2
three  = 3

printAllProbes :: Atom ()
printAllProbes = mapM_ printProbe =<< probes
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


-- | Invoke the atom compiler
compileToC :: IO ()
compileToC = do
    _ <- A.compile "atom_wbs" cfg atom_wbs
    return ()
  where
    cfg = defaults { cCode = prePostCode }
    prePostCode _ _ _ =
      ( unlines [ "#include <stdio.h>"
                , "#include <unistd.h>"
                , ""
                , "// ---- BEGIN of source automatically generated by Atom ----"
                ]
      , unlines [ "// ---- END of source automatically generated by Atom ----"
                , ""
                , "int main(int argc, char **argv) {"
                , "  // call guards() once per second"
                , "  while(1) { atom_wbs(); usleep(1000); }"
                , "}"
                ]
      )

-- Main -----------------------------------------------------------------

putHeader :: IO ()
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
    -- different config from A3
  , ("A3b", atom3, defSpecCfg,
        unwords [ "(query A3b_transition_system"
                , "  (=> A3b_mfa_formula"
                , "    (=> (not (= A3b!atom3!bob!msg (-1))) A3b!atom3!alice!done)))"])
    -- fewer state vars & different property
  , ("A3min", atom3min, defSpecCfg,
        unwords [ "(query A3min_transition_system"
                , "  (=> A3min_mfa_formula"
                , "    (=> (not (= A3min!atom3!bob!msg (-1))) (>= A3min!__t 1))))"])
  , ("A4", atom4, defSpecCfg,
        unwords [ "(query A4_transition_system"
                , "  (=> A4_mfa_formula"
                , "    (=> A4!atom4!nodeC!done (= A4!atom4!nodeC!msg 1))))"
                , "\n\n"
                , "(query A4_transition_system"
                , "  (=> A4_mfa_formula"
                , "    (=> A4!atom4!nodeC!done (= A4!__t 2))))"])
-- Need to add clocks to the language first		
 , ("A7", atom_wbs, defSpecCfg, 
    "(query A1_transition_system (=> A1_mfa_formula (<= 0 A1!atom1!x)))")
  ]

main :: IO ()
main = do
  mapM_ testCompile suite
  compileAtomWBS
  putStrLn "Compiling atom_wbs to C... (atom_wbs.{c,h})"
  compileToC

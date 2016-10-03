{-# LANGUAGE OverloadedStrings #-}

module Main where

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


-- Main -----------------------------------------------------------------

putHeader = putStrLn (replicate 72 '-')

testCompile :: String -> Atom () -> IO ()
testCompile nm atom' = do
  tr <- compile (nameFromS nm) TrConfig atom'
  putSystemLn tr
  putHeader

-- | List of 'Atom's to translate and print
suite :: [(String, Atom ())]
suite = [ ("test_atom1", atom1)
        ]

main :: IO ()
main = do
  putHeader
  mapM_ (uncurry testCompile) suite

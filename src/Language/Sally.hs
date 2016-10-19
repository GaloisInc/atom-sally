-- |
-- Module      :  Language.Sally
-- Copyright   :  Benjamin F Jones 2016
-- License     :  BSD3
--
-- Maintainer  :  benjaminfjones@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Module for exporting the translator API.

module Language.Sally (
  -- * Language.Sally
    compileToSally
  -- * Language.Sally.PPrint
  , hPutSystem
  , putSystemLn
    -- * Language.Sally.Translation
  , translaborate
  , TrConfig(..)
    -- * Language.Sally.Types
  , nameFromS
  , nameFromT
) where

import System.IO
import Language.Atom

import Language.Sally.Types
import Language.Sally.Translation
import Language.Sally.PPrint


-- | Compile an Atom specification to Sally model.
--
-- Configuration and query string are currently optional. The resulting model
-- is written to the filename 'fname' on disk.
compileToSally
  :: String         -- ^ specification name
  -> TrConfig       -- ^ translator configuration
  -> FilePath       -- ^ file to write compiled model to
  -> Atom ()        -- ^ Atom spec to compile
  -> Maybe String   -- ^ (optional) query string to append to the model
  -> IO ()
compileToSally nm config fname spec mQuery = do
  tr <- translaborate (nameFromS nm) config spec
  withFile fname WriteMode $ \h -> do
    hPutSystem tr h
    case mQuery of
      Just q -> do
        hPutStrLn h "\n;; Query"
        hPutStrLn h q
      Nothing -> return ()
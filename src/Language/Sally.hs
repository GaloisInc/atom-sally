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
  -- * Language.Sally.PPrint
    hPutSystem
  , putSystemLn
    -- * Language.Sally.Translation
  , compile
  , TrConfig(..)
    -- * Language.Sally.Types
  , nameFromS
  , nameFromT
) where

import Language.Sally.Types
import Language.Sally.Translation
import Language.Sally.PPrint

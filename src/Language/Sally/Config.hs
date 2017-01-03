-- |
-- Module      :  Language.Sally.Config
-- Copyright   :  Galois Inc. 2016
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Translation configuration, including settings for the fault model,
-- the way variables are rendered, etc.
--
module Language.Sally.Config
  ( TrConfig(..)
  , FaultAssump(..)
  , defaultCfg
  , Weights
  ) where

import Language.Sally.FaultModel
import Language.Sally.Types
import Data.Map.Strict (Map)


-- | Translation configuration, including settings for the fault model.
data TrConfig = TrConfig
  { -- | maximum fault assumption
    cfgMFA :: FaultAssump    -- ^ fault model to use
  , cfgTopNameSpace :: Bool  -- ^ use top-level name space in variables names?
                             -- TODO this doesn't work currently because of
                             -- the way Atom generates names during
                             -- 'elaborate'
  }

-- | Default configuration
defaultCfg :: TrConfig
defaultCfg = TrConfig
  { cfgMFA = NoFaults
  , cfgTopNameSpace = True
  }

-- | Assignment of weights to each fault type
type Weights = Map FaultType Int

-- | Type representing possible fault model assumptions
data FaultAssump =
  -- | No faulty nodes
    NoFaults
  -- | Hybrid faults with weights and a constant term
  -- TODO: elaborate
  | HybridFaults Weights Int
  -- | Fixed configuration of faulty nodes. Nodes not specified are assigned
  -- 'NonFaulty'.
  | FixedFaults (Map Name FaultType)

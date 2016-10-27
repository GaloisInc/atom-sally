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
module Language.Sally.Config (
    TrConfig(..)
  , FaultAssump(..)
  , defaultCfg
) where


-- | Translation configuration, including settings for the fault model.
data TrConfig = TrConfig
  { -- | maximum fault assumption
    cfgMFA :: FaultAssump    -- ^ fault model to use
  , cfgTopNameSpace :: Bool  -- ^ use top-level name space in variables names?
                             -- TODO this doesn't work currently because of
                             -- the way Atom generates names during
                             -- 'elaborate'
  }

defaultCfg :: TrConfig
defaultCfg = TrConfig
  { cfgMFA = NoFaults
  , cfgTopNameSpace = True
  }

data FaultAssump = NoFaults    -- ^ no faulty nodes
                 | MaxByz Int  -- ^ maximum number of byz faulty nodes
                               --   changing on a trace-by-trace basis

              -- Future fault model possibilities:
              --
              -- | MaxHybrid Int  -- weight of byzantine
              --             Int  -- "      "  symmetric
              --             Int  -- "      "  manifest
              --             Int  -- upper bound on weighted sum of faults
              -- | FixedFaults (Map ChannelName Bool)  -- useful?

-- |
-- Module      :  Language.Sally.Types
-- Copyright   :  Benjamin Jones 2016
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Types reflecting the basic Sally input language sections and base types
--
{-# LANGUAGE OverloadedStrings #-}

module Language.Sally.Types (
    -- * Base types
    SallyBaseType
  , SallyConstant
  , Name
    -- * Types for defining transition systems
  , SallyState
  , SallyPred
  , SallyStateFormula
  , SallyTransition
  , SallySystem
) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text

-- | Number of spaces to indent in the pretty printer
n :: Int
n = 2

newtype Name = Name { textFromName :: Text }
  deriving (Show, Eq)

instance Pretty Name where
  pretty = string . textFromName

nameFromString :: String -> Name
nameFromString = Name . T.pack

-- | A defined constant. For our purposes, a real number is represented
-- (approximated) by an exact rational number.
data SallyConstant = SConstBool Bool
                   | SConstInt  Integer
                   | SConstReal Rational
  deriving (Show, Eq)

-- | Base data types in Sally: Booleans, (mathematical) Integers, and
-- (mathematical) Reals
data SallyBaseType = SBool
                   | SInt
                   | SReal
  deriving (Show, Eq)

-- | The state type in Sally
--
-- This consists of 1) a name for the type, 2) a set of state variables (and
-- their associated base type) and, 3) (optionally) a set in input variabels
-- which are uninterpreted in the model; they can be thought of as varying
-- non-deterministically in any system trace.
data SallyState = SallyState
  { stateName      :: Name
  , stateVars      :: [(Name, SallyBaseType)]
  , stateInputVars :: [(Name, SallyBaseType)]
  }
  deriving (Show, Eq)

-- | An AST for predicates
data SallyPred = SPConst Bool
               | SPVar   Name
               | SPAnd   SallyPred SallyPred
               | SPOr    SallyPred SallyPred
               | SPImpl  SallyPred SallyPred
               | SPNot   SallyPred
  deriving (Show, Eq)

-- | A named formula over a state type
data SallyStateFormula = SallyStateFormula
  { stateFormulaName   :: Name
  , stateFormulaDomain :: Name
  , stateFormulaPred   :: SallyPred
  }
  deriving (Show, Eq)

-- | A transition over a given state type
data SallyTransition = SallyTransition
  { transitionName    :: Name
  , transitioniDomain :: Name
  , transitionPred    :: SallyPred
  }
  deriving (Show, Eq)

-- | A transition system declaration
data SallySystem = SallySystem
  { systemStateName :: Name
  , systemInitStateName :: Name
  , systemTransitionName :: Name
  }
  deriving (Show, Eq)

instance Pretty SallySystem where
  pretty (SallySystem { systemStateName = ssn
                     , systemInitStateName = sisn
                     , systemTransitionName = stn
                     })
    = parens (string "define-system" <+> pretty ssn <$$>
              nest n (pretty sisn <$$> pretty stn))

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
    SallyBaseType(..)
  , SallyConst(..)
  , Name
  , nameFromT
  , nameFromS
  , catNames
    -- * Types for defining transition systems
  , SallyState(..)
  , SallyPred(..)
  , SallyStateFormula(..)
  , SallyTransition(..)
  , SallySystem(..)
) where

import Data.String
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text

-- | Number of spaces to indent in the pretty printer
-- nNest :: Int
-- nNest = 2

newtype Name = Name { textFromName :: Text }
  deriving (Show, Eq)

instance Pretty Name where
  pretty = string . textFromName

nameFromS :: String -> Name
nameFromS = Name . T.pack

nameFromT :: Text -> Name
nameFromT = Name

instance IsString Name where
  fromString = nameFromS

-- | Concatenate names with a 'bang' separated in between
catNames :: Name -> Name -> Name
catNames a b = Name (textFromName a `T.append` "!" `T.append` textFromName b)

-- | A defined constant. For our purposes, a real number is represented
-- (approximated) by an exact rational number.
data SallyConst = SConstBool Bool
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
  { sName   :: Name                     -- ^ state type name
  , sVars   :: [(Name, SallyBaseType)]  -- ^ state variables
  , sInVars :: [(Name, SallyBaseType)]  -- ^ state input variables
  }
  deriving (Show, Eq)

-- | An AST for predicates
data SallyPred = SPConst Bool                 -- ^ boolean constant
               | SPVar   Name                 -- ^ state variable
               | SPAnd   SallyPred SallyPred  -- ^ and
               | SPOr    SallyPred SallyPred  -- ^ or
               | SPImpl  SallyPred SallyPred  -- ^ implication
               | SPNot   SallyPred            -- ^ negation
  deriving (Show, Eq)

-- | A named formula over a state type
data SallyStateFormula = SallyStateFormula
  { sfName   :: Name        -- ^ state formula name
  , sfDomain :: Name        -- ^ state formula domain
  , sfPred   :: SallyPred   -- ^ state formula predicate
  }
  deriving (Show, Eq)

-- | A transition over a given state type
data SallyTransition = SallyTransition
  { traName :: Name       -- ^ transition name
  , traDom  :: Name       -- ^ transition domain
  , traPred :: SallyPred  -- ^ transition relation
  }
  deriving (Show, Eq)

-- | A transition system declaration
data SallySystem = SallySystem
  { sysSN  :: Name  -- ^ system state name
  , sysISN :: Name  -- ^ system init state name
  , sysTN  :: Name  -- ^ system transition name
  }
  deriving (Show, Eq)

instance Pretty SallySystem where
    pretty ss = parens (fillSep [ string "define-system"
                                , pretty (sysSN ss)
                                , pretty (sysISN ss)
                                , pretty (sysTN ss)
                                ])

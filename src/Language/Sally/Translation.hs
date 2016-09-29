-- |
-- Module      :  Language.Sally.Translation
-- Copyright   :  Benjamin Jones 2016
-- License     :  BSD3
--
-- Maintainer  :  benjaminfjones@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Translation from Atom's AST to Sally's AST.
--
{-# LANGUAGE OverloadedStrings #-}

module Language.Sally.Translation (
    translate
  , TrResult(..)
) where

import Data.Sequence ((|>))
import qualified Data.Sequence as Seq

import qualified Language.Atom.Elaboration as AEla
import qualified Language.Atom.Expressions as AExp
import qualified Language.Atom.UeMap       as AUe

import Language.Sally.Types


-- Main Translation Code -------------------------------------------------------

-- | The result of translation, a specific form of the Sally AST.
data TrResult = TrResult
  { tresState  :: SallyState
  , tresConsts :: [SallyConst]
  , tresInit   :: SallyStateFormula
  , tresTrans  :: [SallyTransition]
  , tresSystem :: SallySystem
  }

-- | Main translation function.
translate :: TrConfig
          -> Name
          -> AEla.StateHierarchy
          -> AUe.UeMap
          -> [AEla.Rule]
          -> TrResult
translate tconf name hier umap rules =
    TrResult { tresState  = tresState'
             , tresConsts = tresConsts'
             , tresInit   = tresInit'
             , tresTrans  = tresTrans'
             , tresSystem = tresSystem'
             }
  where
    tresState'    = trState name hier
    tresConsts'   =  []  -- TODO support defined constants
    tresInit'     = trInit name hier
    tresTrans'    = trRules name umap rules
    tresSystem'   = trSystem name

trType :: AExp.Type -> SallyBaseType
trType t = case t of
             AExp.Bool   -> SBool
             AExp.Int8   -> SInt
             AExp.Int16  -> SInt
             AExp.Int32  -> SInt
             AExp.Int64  -> SInt
             AExp.Float  -> SReal
             AExp.Double -> SReal
             AExp.Word8  -> errWordN
             AExp.Word16 -> errWordN
             AExp.Word32 -> errWordN
             AExp.Word64 -> errWordN
  where errWordN = error "atom-sally does not support Word8 types"

trTypeConst :: AExp.Const -> SallyBaseType
trTypeConst = trType . AExp.typeOf

trConst :: AExp.Const -> SallyConst
trConst = error "trConst"

trConstE :: AExp.Const -> SallyExpr
trConstE = SEConst . trConst

trName :: AEla.Name -> Name
trName = nameFromS

-- | Produce a state type declaration from the 'StateHierarchy' in Atom.
trState :: Name -> AEla.StateHierarchy -> SallyState
trState name sh = SallyState (mkStateTypeName name) vars invars
  where
    invars = []  -- TODO expose input variables to DSL
    vars = if AEla.isHierarchyEmpty sh then []
           else go (nameFromT "") sh

    go :: Name -> AEla.StateHierarchy -> [(Name, SallyBaseType)]
    go prefix (AEla.StateHierarchy nm items) =
      concatMap (go (prefix `bangNames` (trName nm))) items
    go prefix (AEla.StateVariable nm c) =
      [(prefix `bangNames` (trName nm), trTypeConst c)]
    go prefix (AEla.StateChannel nm c _) =
      let (chanVar, chanReady) = mkChanStateNames (prefix `bangNames` (trName nm))
      in [(chanVar, trTypeConst c), (chanReady, SBool)]
    go prefix (AEla.StateArray _ _) = error "atom-sally does not yet support arrays"

-- | Produce a predicate describing the initial state of the system.
trInit :: Name -> AEla.StateHierarchy -> SallyStateFormula
trInit name sh = SallyStateFormula (mkInitStateName name)
                                   (mkStateTypeName name)
                                   spred
  where
    spred = if AEla.isHierarchyEmpty sh then (SPConst True)
             else go name sh

    go :: Name -> AEla.StateHierarchy -> SallyPred
    go prefix (AEla.StateHierarchy nm items) =
      SPAnd (Seq.fromList $ map (go (prefix `bangNames` (trName nm))) items)
    go prefix (AEla.StateVariable nm c) =
      SPAnd (Seq.fromList [SPEq (SEVar (prefix `bangNames` (trName nm))) (trConstE c)])
    go prefix (AEla.StateChannel nm c b) =
      let (chanVar, chanReady) = mkChanStateNames (prefix `bangNames` (trName nm))
      in SPAnd (  Seq.empty
               |> SPEq (SEVar chanVar) (trConstE c)
               |> SPEq (SEVar chanReady) (trConstE b))
    go prefix (AEla.StateArray _ _) = error "atom-sally does not yet support arrays"

-- | Collect the state type name, initial states name, and master transition
-- name into a 'SallySystem' record.
trSystem :: Name -> SallySystem
trSystem name = SallySystem (mkStateTypeName name)
                            (mkInitStateName name)
                            (mkTransName name)

-- | Translate Atom 'Rule's into 'SallyTransition's. One transition is
-- produced per rule, plus one master transition for use in defining the
-- transition system as a whole.
trRules :: Name -> AUe.UeMap -> [AEla.Rule] -> [SallyTransition]
trRules name umap rules = error "trRules"


-- Name Generation Utilities ---------------------------------------------------

mkStateTypeName :: Name -> Name
mkStateTypeName = (`scoreNames` "state_type")

mkInitStateName :: Name -> Name
mkInitStateName = (`scoreNames` "initial_state")

mkTransName :: Name -> Name
mkTransName = (`scoreNames` "transition")

mkChanStateNames :: Name -> (Name, Name)
mkChanStateNames name = (chanVar, chanReady)
  where chanVar   = name `bangNames` "var"
        chanReady = name `bangNames` "ready"


-- Configuration ---------------------------------------------------------------

-- | TODO define TrConfig
data TrConfig = TrConfig

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
    compile
  , translate
  , TrConfig(..)
) where

import Control.Arrow (second)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as T
import System.Exit

import qualified Language.Atom.Analysis    as AAna
import qualified Language.Atom.Elaboration as AEla
import qualified Language.Atom.Expressions as AExp
import qualified Language.Atom.UeMap       as AUe

import Language.Sally.Types


-- Entry Point from Atom -------------------------------------------------------

-- | Compiles an atom description to Sally. The 'TrResult' can then be printed
-- or written to disk.
compile :: Name
        -> TrConfig
        -> AEla.Atom ()
        -> IO (TrResult)
compile name config atom' = do
  let aname = T.unpack . textFromName $ name
  res <- AEla.elaborate AUe.emptyMap aname atom'
  case res of
   Nothing -> do
     putStrLn "ERROR: Design rule checks failed."
     exitWith (ExitFailure 1)
   Just (umap, (state, rules, _assertionNames, _coverageNames, _probeNames)) -> do
     return (translate config name state umap rules)


-- Main Translation Code -------------------------------------------------------

-- | Main translation function.
translate :: TrConfig
          -> Name
          -> AEla.StateHierarchy
          -> AUe.UeMap
          -> [AEla.Rule]
          -> TrResult
translate _tconf name hier umap rules =
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

-- | Translate types from Atom to Sally. Currently the unsigned int /
-- bitvector types are not supported.
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
trConst (AExp.CBool   x) = SConstBool x
trConst (AExp.CInt8   x) = SConstInt  (fromIntegral x)
trConst (AExp.CInt16  x) = SConstInt  (fromIntegral x)
trConst (AExp.CInt32  x) = SConstInt  (fromIntegral x)
trConst (AExp.CInt64  x) = SConstInt  (fromIntegral x)
trConst (AExp.CWord8  _) = error "trConst: WordN is not supported"
trConst (AExp.CWord16 _) = error "trConst: WordN is not supported"
trConst (AExp.CWord32 _) = error "trConst: WordN is not supported"
trConst (AExp.CWord64 _) = error "trConst: WordN is not supported"
trConst (AExp.CFloat  x) = SConstReal (toRational x)
trConst (AExp.CDouble x) = SConstReal (toRational x)


trConstE :: AExp.Const -> SallyExpr
trConstE = SELit . trConst

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
    go _prefix (AEla.StateArray _ _) = error "atom-sally does not yet support arrays"

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
      SPAnd (Seq.fromList [SPEq (varExpr' (prefix `bangNames` (trName nm)))
                                (trConstE c)])
    go prefix (AEla.StateChannel nm c b) =
      let (chanVar, chanReady) = mkChanStateNames (prefix `bangNames` (trName nm))
      in SPAnd (  Seq.empty
               |> SPEq (varExpr' chanVar) (trConstE c)
               |> SPEq (varExpr' chanReady) (trConstE b))
    go _prefix (AEla.StateArray _ _) = error "atom-sally does not yet support arrays"

-- | Collect the state type name, initial states name, and master transition
-- name into a 'SallySystem' record.
trSystem :: Name -> SallySystem
trSystem name = SallySystem (mkStateTypeName name)
                            (mkInitStateName name)
                            (mkMasterTransName name)

-- | Translate Atom 'Rule's into 'SallyTransition's. One transition is
-- produced per rule, plus one master transition for use in defining the
-- transition system as a whole.
--
-- Note: Assertion and Coverage rules are ignored.
trRules :: Name -> AUe.UeMap -> [AEla.Rule] -> [SallyTransition]
trRules name umap rules = (catMaybes $ map trRule rules) ++ [master]
  where trRule :: AEla.Rule -> Maybe SallyTransition
        trRule r@(AEla.Rule{}) = Just $ SallyTransition (mkTName r)
                                                        (mkStateTypeName name)
                                                        (mkLetBinds r)
                                                        (mkPred r)
        trRule _ = Nothing  -- skip assertions and coverage

        -- master transition is (for now) the disjunction of all minor
        -- transitions
        -- TODO add non-deterministic single transitions
        master = SallyTransition (mkMasterTransName name)
                                 (mkStateTypeName name)
                                 []
                                 (masterPred)
        minorTrans = map (SPExpr . SEVar . varFromName . mkTName) rules
        masterPred = SPOr (Seq.fromList minorTrans)

        mkTName :: AEla.Rule -> Name
        mkTName r@(AEla.Rule{}) = mkTransitionName (AEla.ruleId r) name
        mkTName _ = error "impossible! assert or coverage rule found in mkTName"

        getUEs :: AEla.Rule -> [(AUe.Hash, SallyVar)]
        getUEs r = map (second trExprRef) . AAna.topo umap $ AEla.allUEs r

        mkLetBinds :: AEla.Rule -> [SallyLet]
        mkLetBinds r@(AEla.Rule{}) =
          let ues = getUEs r
          in map (\(h, v) -> (v, trUExpr umap ues h)) ues
        mkLetBinds _ = error "impossible! assert or coverage rule found in mkLetBinds"

        mkPred :: AEla.Rule -> SallyPred
        mkPred r@(AEla.Rule{}) =
          let ues = getUEs r
              lkErr h = "trRules: failed to lookup untyped expr " ++ show h
              lk h = fromMaybe (error $ lkErr h) $ lookup h ues
              handleAssign (muv, h) = case muv of
                AUe.MUV _ n _ -> SPEq (varExpr' (nextName . trName $ n)) (SEVar . lk $ h)
                AUe.MUVArray{}   -> error "trRules: arrays are not supported"
                AUe.MUVExtern{}  -> error "trRules: external vars are not supported"
                AUe.MUVChannel{} -> error "trRules: Chan can't appear in lhs of assign"
              ops = map handleAssign (AEla.ruleAssigns r)
          in SPAnd (Seq.fromList ops)
          -- TODO Important! add next. = state. for all other state vars, i.e.
          --      the ones which are not involved in an assignment
        mkPred _ = error "impossible! assert or coverage rule found in mkPred"


-- Translate Expressions -------------------------------------------------------

trUExpr :: AUe.UeMap -> [(AUe.Hash, SallyVar)] -> AUe.Hash -> SallyExpr
trUExpr umap ues h =
  case AUe.getUE h umap of
    AUe.MUVRef (AUe.MUV _ k _) -> varExpr' (trName k)  -- TODO is this the right name?
    AUe.MUVRef (AUe.MUVArray _ _)  -> aLangErr "arrays"
    AUe.MUVRef (AUe.MUVExtern k _) -> aLangErr $ "external variable " ++ k
    AUe.MUVRef (AUe.MUVChannel _ k _) -> varExpr' (fst . mkChanStateNames $ trName k)
    AUe.MUCast _ _     -> aLangErr "casting"
    AUe.MUConst x      -> SELit (trConst x)
    AUe.MUAdd _ _      -> addExpr a b
    AUe.MUSub _ _      -> subExpr a b
    AUe.MUMul _ _      -> multExpr a b
    AUe.MUDiv _ _      -> aLangErr "division"
    AUe.MUMod _ _      -> aLangErr "modular arithmetic"
    AUe.MUNot _        -> notExpr a
    AUe.MUAnd _        -> andExprs ops
    AUe.MUBWNot _      -> aLangErr "bitwise operations & bitvectors"
    AUe.MUBWAnd  _ _   -> aLangErr "bitwise operations & bitvectors"
    AUe.MUBWOr   _ _   -> aLangErr "bitwise operations & bitvectors"
    AUe.MUBWXor  _ _   -> aLangErr "bitwise operations & bitvectors"
    AUe.MUBWShiftL _ _ -> aLangErr "bitwise operations & bitvectors"
    AUe.MUBWShiftR _ _ -> aLangErr "bitwise operations & bitvectors"
    AUe.MUEq  _ _      -> eqExpr a b
    AUe.MULt  _ _      -> ltExpr a b
    AUe.MUMux _ _ _    -> muxExpr a b c
    AUe.MUF2B _        -> aLangErr "cast to Word32"
    AUe.MUD2B _        -> aLangErr "cast to Word64"
    AUe.MUB2F _        -> aLangErr "cast to Float"
    AUe.MUB2D _        -> aLangErr "cast to Double"
    -- math.h functions are not supported
    AUe.MUPi           -> mathHErr "M_PI"
    AUe.MUExp   _      -> mathHErr "exp"
    AUe.MULog   _      -> mathHErr "log"
    AUe.MUSqrt  _      -> mathHErr "sqrt"
    AUe.MUPow   _ _    -> mathHErr "pow"
    AUe.MUSin   _      -> mathHErr "sin"
    AUe.MUAsin  _      -> mathHErr "asin"
    AUe.MUCos   _      -> mathHErr "cos"
    AUe.MUAcos  _      -> mathHErr "acos"
    AUe.MUSinh  _      -> mathHErr "sinh"
    AUe.MUCosh  _      -> mathHErr "cosh"
    AUe.MUAsinh _      -> mathHErr "asinh"
    AUe.MUAcosh _      -> mathHErr "acosh"
    AUe.MUAtan  _      -> mathHErr "atan"
    AUe.MUAtanh _      -> mathHErr "atanh"
  where lkErr k = "trExpr: failed to lookup untyped expr " ++ show k
        lk k = fromMaybe (error $ lkErr k) $ lookup k ues
        aLangErr s = error $ "trExpr: Atom language feature " ++ s ++ " is not supported"
        mathHErr s = error $ "trExpr: math.h function " ++ s ++ " is not supported"
        ops = map (SEVar . lk) $ AUe.ueUpstream h umap
        a  = head ops
        b  = ops !! 1
        c  = ops !! 2


-- Name Generation Utilities ---------------------------------------------------

-- | name --> name_state_type
mkStateTypeName :: Name -> Name
mkStateTypeName = (`scoreNames` "state_type")

-- | name --> name_initial_state
mkInitStateName :: Name -> Name
mkInitStateName = (`scoreNames` "initial_state")

-- | name --> name_transition
mkMasterTransName :: Name -> Name
mkMasterTransName = (`scoreNames` "transition")

-- | name --> (name!var, name!ready)
mkChanStateNames :: Name -> (Name, Name)
mkChanStateNames name = (chanVar, chanReady)
  where chanVar   = name `bangNames` "var"
        chanReady = name `bangNames` "ready"

-- | i name --> name_transition_i
mkTransitionName :: Int -> Name -> Name
mkTransitionName i name = name `scoreNames` "transition" `scoreNames`
                          nameFromS (show i)

-- | Translate a shared expression reference (an Int) to a variable, e.g.
-- temp!0.
trExprRef :: Int -> SallyVar
trExprRef i = varFromName $ nameFromT "temp" `bangNames` nameFromS (show i)

-- Configuration ---------------------------------------------------------------

-- | TODO define TrConfig
data TrConfig = TrConfig

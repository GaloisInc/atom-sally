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

import qualified Language.Atom.Elaboration as AEla
import qualified Language.Atom.Expressions as AExp
import qualified Language.Atom.UeMap       as AUe

import Language.Sally.Types

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
          -> AEla.StateHierarchy
          -> AUe.UeMap
          -> [AEla.Rule]
          -> TrResult
translate tconf hier umap rules = error "TODO write translate"

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

trName :: AEla.Name -> Name
trName = nameFromS

trState :: Name -> AEla.StateHierarchy -> SallyState
trState name sh = SallyState name vars invars
  where
    invars = []  -- TODO expose input variables to DSL
    vars = if AEla.isHierarchyEmpty sh then []
           else go (nameFromT "") sh

    go :: Name -> AEla.StateHierarchy -> [(Name, SallyBaseType)]
    go prefix (AEla.StateHierarchy nm items) =
      concatMap (go (prefix `catNames` (trName nm))) items
    go prefix (AEla.StateVariable nm c) =
      [(prefix `catNames` (trName nm), trTypeConst c)]
    go prefix (AEla.StateChannel nm c _) =
      let (chanVar, chanReady) = mkChanStateNames (prefix `catNames` (trName nm))
      in [(chanVar, trTypeConst c), (chanReady, SBool)]
    go prefix (AEla.StateArray _ _) = error "atom-sally does not yet support arrays"

mkChanStateNames :: Name -> (Name, Name)
mkChanStateNames name = (chanVar, chanReady)
  where chanVar   = name `catNames` "var"
        chanReady = name `catNames` "ready"

-- declState :: Bool -> StateHierarchy -> String
-- declState define a' = if isHierarchyEmpty a' then ""
--   else
--      (if define then "" else "extern ") ++ init (init (f1 "" a'))
--   ++ (if define then " =\n" ++ f2 "" a' else "") ++ ";\n"
--   where
--   -- render the declaration section
--   -- i :: indentation string
--   -- a :: StateHierarchy
--   f1 i a = case a of
--     StateHierarchy name items ->
--          i ++ "struct {  /* " ++ name ++ " */\n"
--       ++ concatMap (f1 ("  " ++ i)) items ++ i ++ "} " ++ name ++ ";\n"
--     StateVariable  name c     -> i ++ cType (E.typeOf c) ++ " " ++ name ++ ";\n"
--     StateArray     name c     ->
--          i ++ cType (E.typeOf $ head c) ++ " " ++ name ++ "[" ++ show (length c)
--       ++ "];\n"
--     -- render channel value and channel ready flag declarations
--     StateChannel   name c _   ->
--          i ++ cType (E.typeOf c) ++ " " ++ chanVarCName name ++ ";\n"
--       ++ i ++ cType Bool ++ " " ++ chanReadyVarCName name
--       ++ ";\n"

-- render the initialization section
-- i :: indentation string
-- a :: StateHierarchy
--
--  f2 i a = case a of
--    StateHierarchy name items ->
--         i ++ "{  /* " ++ name ++ " */\n"
--      ++ intercalate ",\n" (map (f2 ("  " ++ i)) items) ++ "\n" ++ i ++ "}"
--    StateVariable  name c     -> i ++ "/* " ++ name ++ " */  " ++ showConst c
--    StateArray     name c     ->
--         i ++ "/* " ++ name ++ " */\n" ++ i ++ "{ "
--      ++ intercalate ("\n" ++ i ++ ", ") (map showConst c) ++ "\n" ++ i ++ "}"
--    -- render channel value and channel ready flag initial values
--    StateChannel   name c f   ->
--         i ++ "/* " ++ chanVarCName name ++ " */  " ++ showConst c ++ ",\n"
--      ++ i ++ "/* " ++ chanReadyVarCName name ++ " */ " ++ showConst f

-- --------------------------------------------------------------------------
-- Configuration

-- | TODO define TrConfig
data TrConfig = TrConfig

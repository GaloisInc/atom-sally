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
{-# LANGUAGE ViewPatterns #-}

module Language.Sally.Types (
    -- * Name type
    Name
  , textFromName
  , nameFromT
  , nameFromS
  , catNamesWith
  , bangNames
  , scoreNames
  , nextName
  , stateName
  , inputName
  , varFromName
    -- * Base types
  , SallyBaseType(..)
  , SallyConst(..)
    -- * Types for defining transition systems
  , SallyState(..)
  , SallyPred(..)
  , SallyVar(..)
  , SallyExpr(..)
  , SallyStateFormula(..)
  , SallyLet
  , SallyTransition(..)
  , SallySystem(..)
  , TrResult(..)
    -- * better constructors
  , addExpr
  , subExpr
  , multExpr
  , notExpr
  , eqExpr
  , ltExpr
  , muxExpr
  , andExprs
  , orExprs
  , varExpr
  , varExpr'
  , simplifyAnds
) where

import Data.Foldable (toList)
import Data.Sequence (Seq, (<|), (><), viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.String
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text

import SExpPP


-- Name type for Sally namespaces and variables ------------------------------------

newtype Name = Name { textFromName :: Text }
  deriving (Show, Eq)

instance ToSExp Name where
  toSExp = SXBare . text . textFromName

nameFromS :: String -> Name
nameFromS = Name . T.pack

nameFromT :: Text -> Name
nameFromT = Name

instance IsString Name where
  fromString = nameFromS

-- | Concatenate names with the given seperating text in between
catNamesWith :: Text -> Name -> Name -> Name
catNamesWith sp a b = Name (textFromName a `T.append` sp `T.append` textFromName b)

-- | Concatenate names with a 'bang' separated in between
bangNames :: Name -> Name -> Name
bangNames = catNamesWith "!"

-- | Concatenate names with an 'underscore' separated in between
scoreNames :: Name -> Name -> Name
scoreNames = catNamesWith "_"

-- | Return the name of the given name in the "next" namespace
nextName :: Name -> Name
nextName = catNamesWith "" "next."

-- | Return the name of the given name in the "state" namespace
stateName :: Name -> Name
stateName = catNamesWith "" "state."

-- | Return the name of the given name in the "input" namespace
inputName :: Name -> Name
inputName = catNamesWith "" "input."


-- Constants and base types ----------------------------------------------------

-- | A defined constant. For our purposes, a real number is represented
-- (approximated) by an exact rational number.
data SallyConst = SConstBool Bool
                | SConstInt  Integer
                | SConstReal Rational
  deriving (Show, Eq)

instance ToSExp SallyConst where
  toSExp (SConstBool b) = SXBare $ if b then text "true" else text "false"
  toSExp (SConstInt  x) = SXBare $ integer x
  toSExp (SConstReal x) = SXBare $ rational x

-- | Base data types in Sally: Booleans, (mathematical) Integers, and
-- (mathematical) Reals
data SallyBaseType = SBool
                   | SInt
                   | SReal
  deriving (Show, Eq)

instance ToSExp SallyBaseType where
  toSExp SBool = bareText "Bool"
  toSExp SInt  = bareText "Int"
  toSExp SReal = bareText "Real"

-- Untyped Expression AST for Sally --------------------------------------------

newtype SallyVar = SallyVar { textFromVar :: Text }
  deriving (Show, Eq)

instance ToSExp SallyVar where
  toSExp = SXBare . text . textFromVar

varFromName :: Name -> SallyVar
varFromName = SallyVar . textFromName

-- | Expressions
data SallyExpr = SELit   SallyConst              -- ^ constant literal
               | SEVar   SallyVar                -- ^ variable
               | SEPre   SallyPred               -- ^ boolean expression
               | SEArith SallyArith              -- ^ arithmetic expression
               | SEMux   SallyExpr SallyExpr SallyExpr  -- ^ if then else
  deriving (Show, Eq)

instance ToSExp SallyExpr where
  toSExp (SELit x)   = SXBare (sxPretty x)
  toSExp (SEVar x)   = SXBare (sxPretty x)
  toSExp (SEPre x)   = toSExp x
  toSExp (SEArith x) = toSExp x
  toSExp (SEMux x y z) = SXList [bareText "ite", toSExp x, toSExp y, toSExp z]

-- | Predicates
data SallyPred = SPConst Bool                    -- ^ boolean constant
               | SPExpr  SallyExpr               -- ^ a boolean valued expression
               | SPAnd   (Seq SallyPred)         -- ^ and
               | SPOr    (Seq SallyPred)         -- ^ or
               | SPImpl  SallyPred SallyPred     -- ^ implication
               | SPNot   SallyPred               -- ^ logical negation

               | SPEq    SallyExpr SallyExpr     -- ^ ==

               | SPLEq   SallyExpr SallyExpr   -- ^ <=
               | SPGEq   SallyExpr SallyExpr   -- ^ >=
               | SPLt    SallyExpr SallyExpr   -- ^ <
               | SPGt    SallyExpr SallyExpr   -- ^ >
  deriving (Show, Eq)

instance ToSExp SallyPred where
  toSExp (SPConst x)   = SXBare (text (if x then "true" else "false"))
  toSExp (SPExpr  x)   = SXBare (sxPretty x)
  toSExp (SPAnd   xs)  = SXList (bareText "and" : toList (fmap toSExp xs))
  toSExp (SPOr    xs)  = SXList (bareText "or"  : toList (fmap toSExp xs))
  toSExp (SPImpl  p q) = SXList [bareText "=>", toSExp p, toSExp q]
  toSExp (SPNot   p)   = SXList [bareText "!",  toSExp p]
  toSExp (SPEq    x y) = SXList [bareText "=",  toSExp x, toSExp y]
  toSExp (SPLEq   x y) = SXList [bareText "<=", toSExp x, toSExp y]
  toSExp (SPGEq   x y) = SXList [bareText ">=", toSExp x, toSExp y]
  toSExp (SPLt    x y) = SXList [bareText "<",  toSExp x, toSExp y]
  toSExp (SPGt    x y) = SXList [bareText "<",  toSExp x, toSExp y]

-- | Arithmetic terms
data SallyArith = SAAdd   SallyExpr SallyExpr  -- ^ addition
                | SAMult  SallyExpr SallyExpr  -- ^ constant mult
  deriving (Show, Eq)

instance ToSExp SallyArith where
  toSExp (SAAdd x y)  = SXList [bareText "+", toSExp x, toSExp y]
  toSExp (SAMult x y) = SXList [bareText "*", toSExp x, toSExp y]

-- Better Constructors ---------------------------------------------------------

-- | Better constructor for adding expressions
--   TODO maintain normal form
addExpr :: SallyExpr -> SallyExpr -> SallyExpr
addExpr x y = SEArith (SAAdd x y)

subExpr :: SallyExpr -> SallyExpr -> SallyExpr
subExpr x y = SEArith (SAAdd x ny)
  where ny = multExpr (SELit (SConstInt (-1))) y

-- | Better constructor for multiplying expressions; checks that one of the
-- operands is a constant.
multExpr :: SallyExpr -> SallyExpr -> SallyExpr
multExpr x y = if (isMultConst x || isMultConst y) then SEArith (SAMult x y)
               else error "multExpr: non-linear arithmetic is not supported"

-- | Note: this is an over approximation, e.g. (x + (-x))*y is a constant 0
-- times y, but will not pass this predicate.
isMultConst :: SallyExpr -> Bool
isMultConst (SELit _) = True
isMultConst (SEVar _) = False
isMultConst (SEPre _) = False
isMultConst (SEArith (SAAdd x y))  = isMultConst x && isMultConst y
isMultConst (SEArith (SAMult x y)) = isMultConst x && isMultConst y
isMultConst (SEMux{}) = False

eqExpr :: SallyExpr -> SallyExpr -> SallyExpr
eqExpr x y = SEPre (SPEq x y)

ltExpr :: SallyExpr -> SallyExpr -> SallyExpr
ltExpr x y = SEPre (SPLt x y)

notExpr :: SallyExpr -> SallyExpr
notExpr x = SEPre (SPNot x')
  where x' = case x of
               SEPre w -> w
               _       -> error "notExpr: malformed '!' expression"

muxExpr :: SallyExpr -> SallyExpr -> SallyExpr -> SallyExpr
muxExpr = SEMux

-- TODO efficiently concat sequences if applicable
andExprs :: [SallyExpr] -> SallyExpr
andExprs es = SEPre $ andPreds (fmap getPre es)
  where getPre e = case e of
                     SEPre w -> w
                     _       -> error "andExprs: malformed 'and' expression"

andPreds :: [SallyPred] -> SallyPred
andPreds = SPAnd . flattenAnds . Seq.fromList

flattenAnds :: Seq SallyPred -> Seq SallyPred
flattenAnds (viewl -> xs) =
  case xs of
    EmptyL -> Seq.empty
    a :< rest  ->
      case a of
        SPAnd ys -> flattenAnds ys >< flattenAnds rest
        -- TODO enable rewriting here?
        -- SPConst True  -> flattenAnds rest
        -- SPConst False -> a <| Seq.empty
        _ -> a <| flattenAnds rest

-- | Top-down rewriting of 'and' terms
simplifyAnds :: SallyPred -> SallyPred
simplifyAnds p =
  case p of
    -- main case
    SPAnd xs ->
      let ys = flattenAnds (fmap simplifyAnds xs) :: Seq SallyPred
      in case viewl ys of
           EmptyL  -> SPConst True           -- empty 'and'
           z :< zs -> if Seq.null zs then z  -- single elt. 'and'
                      else SPAnd zs          -- multiple
    SPExpr (SEPre q) -> simplifyAnds q       -- strip off SPExpr . SEPre
    -- other cases
    SPConst _   -> p
    SPOr    xs  -> SPOr (fmap simplifyAnds xs)
    SPImpl  x y -> SPImpl (simplifyAnds x) (simplifyAnds y)
    SPNot   x   -> SPNot (simplifyAnds x)
    _           -> p  -- TODO simplify arithmetic predicates?

orExprs :: [SallyExpr] -> SallyExpr
orExprs es = SEPre $ orPreds (fmap getPre es)
  where getPre e = case e of
                     SEPre w -> w
                     _       -> error "orExprs: malformed 'and' expression"

orPreds :: [SallyPred] -> SallyPred
orPreds = SPOr . Seq.fromList

varExpr :: SallyVar -> SallyExpr
varExpr = SEVar

varExpr' :: Name -> SallyExpr
varExpr' = SEVar . varFromName

-- Compound Sally Types --------------------------------------------------------

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

instance ToSExp SallyState where
  toSExp (SallyState {sName=sn, sVars=sv, sInVars=siv}) =
    SXList $ [ bareText "define-state"
             , toSExp sn
             , SXList $ map (\(n,t) -> SXList [toSExp n, toSExp t]) sv
             ] ++
             (if null siv then []
              else [SXList $ map (\(n,t) -> SXList [toSExp n, toSExp t]) siv])

-- | A named formula over a state type
data SallyStateFormula = SallyStateFormula
  { sfName   :: Name        -- ^ state formula name
  , sfDomain :: Name        -- ^ state formula domain
  , sfPred   :: SallyPred   -- ^ state formula predicate
  }
  deriving (Show, Eq)

instance ToSExp SallyStateFormula where
  toSExp (SallyStateFormula {sfName=sn, sfDomain=sd, sfPred=sp}) =
    SXList [ bareText "define-state-formula"
           , toSExp sn
           , toSExp sd
           , toSExp sp
           ]

-- | A "let" binding: each let binds a 'SallyVar' to a Sally expression,
-- which can be a constant literal, a predicate (boolean value), or an
-- arithmetic expression.
type SallyLet = (SallyVar, SallyExpr)

-- | A transition over a given state type
data SallyTransition = SallyTransition
  { traName :: Name        -- ^ transition name
  , traDom  :: Name        -- ^ transition domain
  , traLet  :: [SallyLet]  -- ^ bindings for the transition relation
  , traPred :: SallyPred   -- ^ transition relation
  }
  deriving (Show, Eq)

instance ToSExp SallyTransition where
  toSExp (SallyTransition {traName=tn, traDom=td, traLet=tl, traPred=tp}) =
      SXList [ bareText "define-transition"
             , toSExp tn
             , toSExp td
             , SXList [bareText "let", SXList listOfBinds, toSExp tp]
             ]
    where
      listOfBinds = map (\(v,e) -> SXList [toSExp v, toSExp e]) tl

-- | A transition system declaration
data SallySystem = SallySystem
  { sysSN  :: Name  -- ^ system state name
  , sysISN :: Name  -- ^ system init state name
  , sysTN  :: Name  -- ^ system transition name
  }
  deriving (Show, Eq)

instance ToSExp SallySystem where
    toSExp ss = SXList [ bareText "define-system"
                       , toSExp (sysSN ss)
                       , toSExp (sysISN ss)
                       , toSExp (sysTN ss)
                       ]


-- Translation Results ---------------------------------------------------------

-- | The result of translation, a specific form of the Sally AST.
data TrResult = TrResult
  { tresState  :: SallyState
  , tresConsts :: [SallyConst]
  , tresInit   :: SallyStateFormula
  , tresTrans  :: [SallyTransition]
  , tresSystem :: SallySystem
  }
  deriving (Show, Eq)

-- | TrResult requires a special printer since it is not an s-expression
instance Pretty TrResult where
  pretty tr = vcat [ consts_comment
                   , consts
                   , state_comment
                   , sxPretty (tresState tr)
                   , init_comment
                   , sxPretty (tresInit tr)
                   ] <$$>
              vcat (trans_comment : map sxPretty (tresTrans tr)) <$$>
              vcat (system_comment : [sxPretty (tresSystem tr)])
    where
      consts = if null (tresConsts tr) then text ";; NONE"
               else vcat (map sxPretty (tresConsts tr))
      consts_comment = text ";; Constants"
      state_comment  = text "\n;; State type"
      init_comment   = text "\n;; Initial State"
      trans_comment  = text "\n;; Transitions"
      system_comment = text "\n;; System Definition"


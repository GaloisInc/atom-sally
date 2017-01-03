-- |
-- Module      :  Language.Sally.Expr
-- Copyright   :  Benjamin Jones <bjones@galois.com> 2016-2017
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Better constructors for Sally expresssions and predicates than the raw ones
-- defined in "Language.Sally.Types".
--
module Language.Sally.Expr (
    -- * better constructors
    boolExpr
  , boolPred
  , intExpr
  , zeroExpr
  , oneExpr
  , realExpr
  , addExpr
  , subExpr
  , multExpr
  , notExpr
  , eqExpr
  , neqExpr
  , ltExpr
  , leqExpr
  , gtExpr
  , geqExpr
  , muxExpr
  , andExprs
  , orExprs
  , varExpr
  , varExpr'
  -- * complex expression builders
  , minExpr
  , countExpr
) where

import qualified Data.Sequence as Seq
import Language.Sally.Types


-- Better Constructors ---------------------------------------------------------

boolExpr :: Bool -> SallyExpr
boolExpr = SELit . SConstBool

boolPred :: Bool -> SallyPred
boolPred = SPConst

intExpr :: Integral a => a -> SallyExpr
intExpr = SELit . SConstInt . fromIntegral

zeroExpr :: SallyExpr
zeroExpr = intExpr (0 :: Int)

oneExpr :: SallyExpr
oneExpr = intExpr (1 :: Int)

realExpr :: Real a => a -> SallyExpr
realExpr = SELit . SConstReal . toRational


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

leqExpr :: SallyExpr -> SallyExpr -> SallyExpr
leqExpr x y = SEPre (SPLEq x y)

gtExpr :: SallyExpr -> SallyExpr -> SallyExpr
gtExpr x y = SEPre (SPGt x y)

geqExpr :: SallyExpr -> SallyExpr -> SallyExpr
geqExpr x y = SEPre (SPGEq x y)

notExpr :: SallyExpr -> SallyExpr
notExpr x = SEPre (SPNot (getPred x))

neqExpr :: SallyExpr -> SallyExpr -> SallyExpr
neqExpr x y = notExpr (eqExpr x y)

-- | Turn a SallyExpr into a SallyPred (if possible)
getPred :: SallyExpr -> SallyPred
getPred x = case x of
              SEPre w   -> w
              SELit{}   -> SPExpr x
              SEVar{}   -> SPExpr x
              SEMux{}   -> SPExpr x
              SEArith{} -> error ("notExpr: cannot turn expression into predicate: "
                                 ++ show x)

muxExpr :: SallyExpr -> SallyExpr -> SallyExpr -> SallyExpr
muxExpr = SEMux

andExprs :: [SallyExpr] -> SallyExpr
andExprs es = SEPre $ andPreds (fmap getPred es)

andPreds :: [SallyPred] -> SallyPred
andPreds = SPAnd . flattenAnds . Seq.fromList

orExprs :: [SallyExpr] -> SallyExpr
orExprs es = SEPre $ orPreds (fmap getPred es)

orPreds :: [SallyPred] -> SallyPred
orPreds = SPOr . flattenOrs . Seq.fromList

varExpr :: SallyVar -> SallyExpr
varExpr = SEVar

varExpr' :: Name -> SallyExpr
varExpr' = SEVar . varFromName


-- More Complicated expression builders ----------------------------------------

-- | Given a non-empty finite list of expressions, build an expression to
-- compute their minimum. The second argument is a special value which, if
-- present causes expressions in the list with this value to be ignored in the
-- calculation. If the input list contains only the special value, then the
-- special value itself is returned.
minExpr :: [SallyExpr] -> Maybe SallyExpr -> SallyExpr
minExpr [] _ = error "minExpr: cannot apply minExpr to empty list"
minExpr (x:rest) sp' = go sp' x rest
  where go _ m [] = m
        go Nothing m (y:more) = muxExpr (ltExpr m y)
                                        (go sp' m more)
                                        (go sp' y more)
        go (Just sp) m (y:more) = muxExpr (andExprs [ltExpr m y, neqExpr m sp])
                                          (go sp' m more)
                                          (go sp' y more)

-- | Build a Sally expression representing the number of times a particular
-- item appears in a list of expressions.
countExpr :: SallyExpr -> [SallyExpr] -> SallyExpr
countExpr _ [] = zeroExpr
countExpr x (y:rest) = muxExpr (eqExpr x y) (addExpr oneExpr (countExpr x rest))
                                            (countExpr x rest)

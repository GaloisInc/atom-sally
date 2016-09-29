-- |
-- Module      :  Language.Sally.PPrint
-- Copyright   :  Benjamin Jones 2016
-- License     :  BSD3
--
-- Maintainer  :  benjaminfjones@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Exports a pretty printer for the main Sally types
--
module Language.Sally.PPrint (
  pprintSystem
) where

import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.Leijen.Text

import Language.Sally.Types

pprintSystem :: SallySystem -> TL.Text
pprintSystem = displayT . renderPretty ribbon wid . pretty
  where ribbon = 72 / 80 :: Float
        wid    = 80

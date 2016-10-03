-- |
-- Module      :  Language.Sally.PPrint
-- Copyright   :  Benjamin Jones 2016
-- License     :  BSD3
--
-- Maintainer  :  benjaminfjones@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Exports a pretty printer for the result of translation.
--
module Language.Sally.PPrint (
    pprintSystem
  , putSystem
  , putSystemLn
  , hPutSystem
) where

import qualified Data.Text.Lazy as TL
import System.IO (Handle)
import Text.PrettyPrint.Leijen.Text

import Language.Sally.Types

pprintSystem :: TrResult -> TL.Text
pprintSystem = displayT . renderPretty ribbon wid . pretty
  where ribbon = 72 / 80 :: Float
        wid    = 80

putSystem :: TrResult -> IO ()
putSystem = putDoc . pretty

putSystemLn :: TrResult -> IO ()
putSystemLn tr = putSystem tr >> putStrLn ""

hPutSystem :: TrResult -> Handle -> IO ()
hPutSystem tr h = hPutDoc h (pretty tr)

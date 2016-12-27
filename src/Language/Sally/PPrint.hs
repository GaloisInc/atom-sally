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
  -- * pretty printing
    pprintSystem
  , putSystem
  , putSystemLn
  , hPutSystem
) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy.Char8 as BS

import System.IO (Handle)
import Text.PrettyPrint.Leijen.Text

import Language.Sally.Types

pprintSystem :: TrResult -> L.Text
pprintSystem = displayT . renderPretty ribbon wid . pretty
  where ribbon = 72 / 80 :: Float
        wid    = 80

putSystem :: TrResult -> IO ()
putSystem = putDoc . pretty

putSystemLn :: TrResult -> IO ()
putSystemLn tr = putSystem tr >> putStrLn ""

hPutSystem :: Handle -> TrResult -> IO ()
hPutSystem h tr = BS.hPutStr h . E.encodeUtf8 . pprintSystem $ tr

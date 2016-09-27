module Language.Sally.PPrint (
  pprintSystem
) where

import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.Leijen.Text

import Language.Sally.Types

pprintSystem :: SallySystem -> TL.Text
pprintSystem = displayT . renderPretty ribbon width . pretty
  where ribbon = 72 / 80 :: Float
        width  = 80

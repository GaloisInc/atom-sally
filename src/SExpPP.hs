module SExpPP (
    SExp(..)
  , ToSExp(..)
  , bareText
) where


import Data.Text.Lazy (Text)
import Text.PrettyPrint.Leijen.Text

hang' :: Doc -> Doc
hang' =  hang 2

data SExp = SXBare Doc     -- ^ bare symbol or literal
          | SXList [SExp]  -- ^ (foo a b)

class ToSExp a where
  toSExp :: a -> SExp

  sxPretty :: a -> Doc
  sxPretty = sxPrettyDefault . toSExp

instance ToSExp SExp where
  toSExp = id

sxPrettyDefault :: SExp -> Doc
sxPrettyDefault (SXBare x) = x
sxPrettyDefault (SXList []) = lparen <> rparen
sxPrettyDefault (SXList ll@(x:_)) = case x of
  SXBare _ -> parens (hang' (fillSep (map sxPretty ll)))
  SXList _ -> parens (fillSep (map sxPretty ll))

bareText :: Text -> SExp
bareText = SXBare . text

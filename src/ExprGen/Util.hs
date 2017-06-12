module ExprGen.Util
  ( asText
  , str
  , (<>?)
  , (<>)
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Text (Text, pack)
import Data.Monoid ((<>))

asText :: Show a => a -> Text
asText = pack . show

-- |Conditional append
(<>?) :: Monoid m => m -> Bool -> m -> m
(<>?) t b r = if b then t <> r else t

-- |Multiline string
str :: QuasiQuoter
str = QuasiQuoter
  { quoteDec = error "multiline strings are not allowed to be in declarations"
  , quotePat = error "multiline strings are not allowed to be in patterns"
  , quoteType = error "multiline strings are not allowed to be in type signatures"
  , quoteExp = stringE
  }

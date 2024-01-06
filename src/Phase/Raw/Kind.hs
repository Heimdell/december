
module Phase.Raw.Kind where

import Ignored
import Show'

data Kind
  = KStar  I
  | KArrow I Kind Kind

instance Show' Kind where
  show' p = \case
    KStar  _     -> "*"
    KArrow _ d c -> pr p 5 (show' 6 d <> " => " <> show' 5 c)
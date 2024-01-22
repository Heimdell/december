
module Ignored where

import Text.Parser.Yard.Point ( Point )

newtype Ign a = Ign { raw :: a }

instance Eq (Ign a) where
  _ == _ = True

instance Ord (Ign a) where
  compare _ _ = mempty

instance Show (Ign a) where
  show _ = "#"

type I = Ign Point
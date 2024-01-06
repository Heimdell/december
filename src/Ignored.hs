
module Ignored where

import Text.Parser.Yard.Point ( Point )

newtype Ign a = Ign { raw :: a }

instance Eq (Ign a) where
  (==) :: Ign a -> Ign a -> Bool
  _ == _ = True

instance Ord (Ign a) where
  compare :: Ign a -> Ign a -> Ordering
  compare _ _ = mempty

instance Show (Ign a) where
  show _ = "#"

type I = Ign Point
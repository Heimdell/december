
module Phase.Typed.Kind where

import GHC.Generics

import Text.Parser.Yard.Point

import Ignored
import Control.Unification
import Name

data Kind_ it
  = KStar_  (Ign Point)
  | KArrow_ (Ign Point) it it
  deriving stock    (Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Unifiable)

newtype KVar_ = KVar_ { name :: Name }
  deriving newtype (Eq, Ord, Show)

type Kind = Term Kind_ KVar_

pattern KStar  i     = Struct (KStar_  (Ign i))
pattern KArrow i d c = Struct (KArrow_ (Ign i) d c)
pattern KVar     n   = Var    (KVar_           n)
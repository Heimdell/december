
module Phase.Typed.Kind where

import GHC.Generics

import Ignored
import Control.Unification
import Name

data Kind_ i it
  = KStar_  (Ign i)
  | KArrow_ (Ign i) it it
  deriving stock    (Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Unifiable)

newtype KVar_ = KVar_ { name :: Name }
  deriving newtype (Eq, Ord, Show)

type Kind i = Term (Kind_ i) KVar_

pattern KStar  i     = Struct (KStar_  (Ign i))
pattern KArrow i d c = Struct (KArrow_ (Ign i) d c)
pattern KVar     n   = Var    (KVar_           n)
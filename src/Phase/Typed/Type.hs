
module Phase.Typed.Type where

import GHC.Generics

import Ignored
import Control.Unification
import Name

data Type_ i it
  = TConst_ (Ign i) Name
  | TApp_   (Ign i) it it
  | TArrow_ (Ign i) it it
  deriving stock    (Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Unifiable)

newtype TVar_ = TVar_ { name :: Name }
  deriving newtype (Eq, Ord, Show)

type Type i = Term (Type_ i) TVar_

pattern TConst i n   = Struct (TConst_ (Ign i) n)
pattern TArrow i d c = Struct (TArrow_ (Ign i) d c)
pattern TVar     n   = Var    (TVar_           n)

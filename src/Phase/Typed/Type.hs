
module Phase.Typed.Type where

import GHC.Generics

import Text.Parser.Yard.Point

import Ignored
import Control.Unification
import Name

data Type_ it
  = TApp_   (Ign Point) it it
  | TArrow_ (Ign Point) it it
  deriving stock    (Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Unifiable)

newtype TVar_ = TVar_ { name :: Name }
  deriving newtype (Eq, Ord, Show)

type Type = Term Type_ TVar_

pattern TApp   i f x = Struct (TApp_   (Ign i) f x)
pattern TArrow i d c = Struct (TArrow_ (Ign i) d c)
pattern TVar     n   = Var    (TVar_           n)

data TypeExpr_ n
  = Record (Ign Point) [(Name, Term Type_ n)]
  | Union  (Ign Point) [(Name, Term Type_ n)]

type TypeExpr = TypeExpr_ TVar_
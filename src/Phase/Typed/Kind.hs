
module Phase.Typed.Kind where

import GHC.Generics

import Text.Parser.Yard.Point

import Ignored
import Control.Unification
import Name
import Show'

data Kind_ it
  = KStar_
  | KClass_
  | KArrow_ it it
  deriving stock    (Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Unifiable)

newtype KVar_ = KVar_ { name :: TName }
  deriving newtype (Eq, Ord, Show)

type Kind = Term Kind_ KVar_

pattern KStar, KClass :: Kind
pattern KStar      = Struct  KStar_
pattern KClass     = Struct  KClass_
pattern KArrow :: Kind -> Kind -> Kind
pattern KArrow d c = Struct (KArrow_ d c)
pattern KVar :: TName -> Kind
pattern KVar   n   = Var    (KVar_   n)

instance Show' a => Show' (Kind_ a) where
  show' p = \case
    KStar_   -> "*"
    KClass_   -> "#"
    KArrow_ d c -> pr p 5 (show' 6 d <> " => " <> show' 5 c)
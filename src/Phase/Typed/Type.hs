
module Phase.Typed.Type where

import GHC.Generics ( Generic1 )
import Data.Map qualified as Map

import Text.Parser.Yard.Point

import Ignored
import Control.Unification
import Name
import Show'
import Ignored
import Data.Maybe (fromJust, fromMaybe)

data Type_ it
  = TApp_   it it
  | TArrow_ it it
  | TConst_ TName
  deriving stock    (Functor, Foldable, Traversable, Generic1, Eq, Ord)
  deriving anyclass (Unifiable)

newtype TVar_ = TVar_ { name :: TUName }
  deriving newtype (Eq, Ord, Show', IsName)

type Type = Term Type_ TVar_

pattern TApp   f x = Struct (TApp_   f x)
pattern TArrow d c = Struct (TArrow_ d c)
pattern TVar   n   = Var    (TVar_   n)
pattern TConst n   = Struct (TConst_ n)

data TypeExpr_ n
  = Record (Map.Map FName (Term Type_ n))
  | Union  (Map.Map CName (Term Type_ n))

type TypeExpr = TypeExpr_ TVar_

instance Show' n => Show' (TypeExpr_ n) where
  show' p = \case
    Record xs -> "{" <> show' 0 xs <> "}"
    Union  xs -> "<" <> show' 0 xs <> ">"

data Rank1Base n = Rank1Base
  { body ::  Term Type_ n
  , ctx  :: [Term Type_ n]
  }
  deriving stock (Functor, Foldable, Traversable)

type Rank1_ n = Scheme Rank1Base n
type Rank1    = Rank1_ TVar_

type Rank1TypeExpr_ n = Scheme TypeExpr_ n
type Rank1TypeExpr    = Rank1TypeExpr_ TVar_

instance Show' n => Show' (Rank1Base n) where
  show' _ r1b = show r1b.body <> " when " <> punctuate ", " (map show r1b.ctx)

instance Show' n => Show' (Type_ n) where
  show' p = \case
    TArrow_ d c -> pr p 5 (show' 6 d <> " -> " <> show' 5 c)
    TApp_   f x -> pr p 4 (show' 4 f <> " "    <> show' 5 x)
    TConst_ n   -> show n

-- substitute :: [(TVar_, Type)] -> Type -> Type
-- substitute ctx = go
--   where
--     go = \case
--       TApp   f x -> TApp (go f) (go x)
--       TArrow f x -> TArrow (go f) (go x)
--       Var    v   -> Var v
--       TConst   n -> fromMaybe (TConst n) (lookup (TVar_ n) ctx)

substituteTVars :: [(TVar_, Type)] -> Type -> Type
substituteTVars ctx = go
  where
    go = \case
      TApp   f x -> TApp (go f) (go x)
      TArrow f x -> TArrow (go f) (go x)
      TConst   v -> TConst v
      Var    n   -> fromJust (lookup n ctx)

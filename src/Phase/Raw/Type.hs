
module Phase.Raw.Type where

import Data.Map qualified as Map

import Ignored
import Show'
import Name
import Phase.Raw.Kind

data Type
  = TArrow I Type Type
  | TApp   I Type Type
  | TConst I TName
  | TVar   I TUName
  | TEq    I Type Type

instance Show' Type where
  show' p = \case
    TArrow _ d c -> pr p 5 (show' 6 d <> " -> " <> show' 5 c)
    TApp   _ f x -> pr p 4 (show' 4 f <> " "    <> show' 5 x)
    TConst _ n   -> show n
    TVar   _ n   -> "`" <> show n
    TEq    _ t u -> pr p 6 (show' 6 t <> " ~ " <> show' 6 u)

data Rank1 = Rank1
  { point    :: I
  , typeVars :: [TUName]
  , body     :: Type
  , ctx      :: [Type]
  }

instance Show Rank1 where
  show s =
    if null s.typeVars
    then show s.body
    else "type " <> unwords (map show s.typeVars) <> ". " <> show s.body
      <> if null s.ctx then "" else " when " <> punctuate ", " (map show s.ctx)

data TypeExpr
  = Record I (Map.Map FName Type)
  | Union  I (Map.Map CName Type)

instance Show TypeExpr where
  show = \case
    Record _ fields -> "{" <> show' 0 fields <> "}"
    Union  _ ctors  -> "<" <> show' 0 ctors  <> ">"

data TypeSig = TypeSig
  { name :: TName
  , kind :: Kind
  }

instance Show TypeSig where
  show td = "kind " <> show td.name <> " : " <> show td.kind

data TypeDecl = TypeDecl
  { point    :: I
  , name     :: TName
  , typeVars :: [TUName]
  , body     :: TypeExpr
  }

instance Show TypeDecl where
  show td = "type " <> show td.name <> " " <> unwords (map show td.typeVars) <> " =\n"
    <> indent (show td.body)
    where

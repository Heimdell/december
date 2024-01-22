
module Phase.Raw.Toplevel where

import Data.Text qualified as Text
import Data.Map qualified as Map

import Name
import Show'
import Ignored

import Phase.Raw.Type
import Phase.Raw.Expr
import Phase.Raw.Classes
import Data.Function ((&))
import Pass.TypeCheck.Expr (CanTCExprs)

data Imported
  = Typename TName
  | Value    VName

instance Show Imported where
  show = \case
    Typename t -> show t
    Value    v -> show v

data Import = Import
  { point   :: I
  , module_ :: MName
  , items   :: [Imported]
  }

instance Show Import where
  show i = "import " <> show i.module_ <> " using (" <> punctuate ", " (map show i.items) <> ")"

data TopDecl
  = ATypeSig  I TypeSig
  | ATypeDecl I TypeDecl
  | ADeclSig  I Sig
  | ADecl     I Decl
  | Klass     I KlassDecl
  | Instance  I InstanceDecl

instance Show TopDecl where
  show = \case
    ATypeSig  _ sig -> show sig <> "\n"
    ATypeDecl _ d   -> show d   <> "\n"
    ADeclSig  _ s   -> show s   <> "\n"
    ADecl     _ d   -> show d   <> "\n"
    Klass _ k -> show k <> "\n"
    Instance _ i -> show i <> "\n"

data Prog = Prog
  { name    :: MName
  , imports :: [Import]
  , decls   :: [TopDecl]
  }

instance Show Prog where
  show p =
    "module " <> show p.name <> " where\n"
      <> unlines (map show p.imports)
      <> unlines (map show p.decls)

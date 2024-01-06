
module Phase.Raw where

import Data.Text qualified as Text
import GHC.Generics

import Text.Parser.Yard hiding (Expr)
import Control.Unification

import Show'
import Name
import Ignored

data Imported
  = Typename Name
  | Value    Name

instance Show Imported where
  show = \case
    Typename t -> show t
    Value    v -> show v

data Import = Import
  { point   :: I
  , module_ :: Name
  , items   :: [Imported]
  }

instance Show Import where
  show i = "import " <> show i.module_ <> " using (" <> punctuate ", " (map show i.items) <> ")"

data Kind
  = KStar  I
  | KArrow I Kind Kind

instance Show' Kind where
  show' p = \case
    KStar  _     -> "*"
    KArrow _ d c -> pr p 5 (show' 6 d <> " => " <> show' 5 c)

data Type
  = TArrow I Type Type
  | TApp   I Type Type
  | TConst I Name

instance Show' Type where
  show' p = \case
    TArrow _ d c -> pr p 5 (show' 6 d <> " -> " <> show' 5 c)
    TApp   _ f x -> pr p 4 (show' 4 f <> " "    <> show' 5 x)

data Rank1 = Rank1
  { point    :: I
  , typeVars :: [Name]
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
  = Record I [(Name, Type)]
  | Union  I [(Name, Type)]

instance Show TypeExpr where
  show = \case
    Record _ fields -> "{" <> punctuate ", " (map semi fields) <> "}"
    Union  _ ctors  -> "<" <> punctuate ", " (map semi ctors)  <> ">"

data TypeSig = TypeSig
  { name :: Name
  , kind :: Kind
  }

instance Show TypeSig where
  show td = "kind " <> show td.name <> " : " <> show td.kind

data TypeDecl = TypeDecl
  { name     :: Name
  , kindVars :: [Name]
  , body     :: TypeExpr
  }

instance Show TypeDecl where
  show td = "type " <> show td.name <> " " <> unwords (map show td.kindVars) <> " =\n"
    <> indent (show td.body)
    where

data Constant
  = I I Integer
  | F I Double
  | S I Text.Text

instance Show Constant where
  show = \case
    I _ i -> show i
    F _ i -> show i
    S _ i -> show i

data Pattern
  = PCtor  I Name Name  -- ctor var

instance Show Pattern where
  show = \case
    PCtor  _ s v -> show s <> " " <> show v

data Alt = Alt
  { point :: I
  , pat   :: Pattern
  , body  :: Expr
  }

instance Show Alt where
  show alt = show alt.pat <> " -> " <> show alt.body

data LocalDef
  = LSig  Sig
  | LDecl Decl

instance Show LocalDef where
  show = \case
    LSig  s -> show s
    LDecl s -> show s

data Expr
  = Let    I [LocalDef] Expr

  | App    I Expr Expr
  | Lambda I [Name] Expr

  | Object I [(Name, Expr)]
  | Get    I Expr Name
  | Update I Expr [(Name, Expr)]

  | Symbol I Name Expr
  | Case   I Expr [Alt]

  | EVar    I Name

  | Const  I Constant

instance Show' Expr where
  show' p = \case
    Let _ decls k -> "let\n" <> indent (unlines (map show decls)) <> show k
    App _ f x     -> pr p 4 (show' 4 f <> " "    <> show' 5 x)
    Lambda _ xs b -> pr p 3 ("\\" <> punctuate ", " (map show xs) <> " -> " <> show b)
    Object _ fs   -> "{" <> punctuate ", " (map semi fs) <> "}"
    Get    _ o f  -> show' 3 o <> "." <> show f
    Update _ o fs -> "with " <> show o <> " {" <> punctuate ", " (map semi fs) <> "}"
    Symbol _ c x  -> show c <> " " <> show' 3 x
    Case   _ o as -> "case " <> show o <> " {\n" <> indent (punctuate ";\n" (map show as)) <> "}"
    EVar    _ v    -> show v
    Const  _ c    -> show c

data Sig = Sig
  { name :: Name
  , sig  :: Rank1
  }

instance Show Sig where
  show d = "sig " <> show d.name <> " : " <> show d.sig

data Decl = Decl
  { name :: Name
  , body :: Expr
  }

instance Show Decl where
  show d = "val " <> show d.name <> " = " <> show d.body

data KlassDecl = KlassDecl
  { name   :: Name
  , args   :: [Name]
  , deps   :: [Type]
  , fields :: [(Name, Rank1)]
  }

instance Show KlassDecl where
  show kd = "class " <> show kd.name <> " " <> unwords (map show kd.args) <> " when\n"
    <> indent (punctuate ",\n" (map show kd.deps)) <> "{\n"
    <> indent (unlines (map semi kd.fields))
    <> "}"

data InstanceDecl = InstanceDecl
  { point  :: I
  , header :: Rank1
  , deps   :: [Type]
  , impls  :: [(Name, Expr)]
  }

instance Show InstanceDecl where
  show id =
    "instance " <> show id.header <> " when\n"
      <> indent (unlines (map show id.deps))
      <> "{\n"
      <> indent (unlines (map assign id.impls))
      <> "}"

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
  { name    :: Name
  , imports :: [Import]
  , decls   :: [TopDecl]
  }

instance Show Prog where
  show p =
    "module " <> show p.name <> " where\n"
      <> unlines (map show p.imports)
      <> unlines (map show p.decls)

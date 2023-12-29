
{-# LANGUAGE NoFieldSelectors #-}
module AST where

import Data.Text qualified as Text
import Text.Parser.Yard.Point

class Show' a where
  show' :: Int -> a -> String

instance {-# OVERLAPPABLE #-} Show' a => Show a where
  show = show' 0

pr :: Int -> Int -> String -> String
pr p n str = if p > n then "(" <> str <> ")" else str

punctuate :: String -> [String] -> String
punctuate sep []       = ""
punctuate sep [x]      = x
punctuate sep [x, y]   = x <> sep <> y
punctuate sep (x : xs) = x <> sep <> punctuate sep xs

indent :: String -> String
indent = unlines . map ("  " ++) . lines

semi (v, k) = show v <> ": " <> show k

assign (v, k) = show v <> " = " <> show k

data Name = Name
  { point :: Point
  , raw   :: Text.Text
  }

instance Show Name where
  show n = Text.unpack n.raw

newtype VName = VName { name :: Name } deriving newtype (Show)
newtype TName = TName { name :: Name } deriving newtype (Show)
newtype CName = CName { name :: Name }
newtype FName = FName { name :: Name } deriving newtype (Show)
newtype MName = MName { name :: Name } deriving newtype (Show)

instance Show CName where
  show n = "#" <> show n.name

data Imported
  = Typename TName
  | Value    VName

instance Show Imported where
  show = \case
    Typename t -> show t
    Value    v -> show v

data Import = Import
  { point   :: Point
  , module_ :: MName
  , items   :: [Imported]
  }

instance Show Import where
  show i = "import " <> show i.module_ <> " using (" <> punctuate ", " (map show i.items) <> ")"

data Kind
  = KVar   Point VName
  | KStar  Point
  | KArrow Point Kind Kind

instance Show' Kind where
  show' p = \case
    KStar  _     -> "*"
    KArrow _ d c -> pr p 5 (show' 6 d <> " => " <> show' 5 c)

data Type
  = TVar   Point VName
  | TConst Point TName
  | TArrow Point Type Type
  | TApp   Point Type Type

instance Show' Type where
  show' p = \case
    TVar   _ v   -> show v
    TConst _ t   -> show t
    TArrow _ d c -> pr p 5 (show' 6 d <> " -> " <> show' 5 c)
    TApp   _ f x -> pr p 4 (show' 4 f <> " "    <> show' 5 x)

data Scheme = Scheme
  { point    :: Point
  , typeVars :: [VName]
  , body     :: Type
  , ctx      :: [Type]
  }

instance Show Scheme where
  show s =
    if null s.typeVars
    then show s.body
    else "type " <> unwords (map show s.typeVars) <> ". " <> show s.body
      <> if null s.ctx then "" else " when " <> punctuate ", " (map show s.ctx)

data TypeExpr
  = Record Point [(FName, Type)]
  | Union  Point [(CName, Type)]

instance Show TypeExpr where
  show = \case
    Record _ fields -> "{" <> punctuate ", " (map semi fields) <> "}"
    Union  _ ctors  -> "<" <> punctuate ", " (map semi ctors)  <> ">"

data TypeDecl = TypeDecl
  { name     :: TName
  , kindVars :: [(VName, Kind)]
  , body     :: TypeExpr
  }

instance Show TypeDecl where
  show td = "type " <> show td.name <> " " <> unwords (map semi td.kindVars) <> " =\n"
    <> indent (show td.body)
    where

data Constant
  = I Point Integer
  | F Point Double
  | S Point Text.Text

instance Show Constant where
  show = \case
    I _ i -> show i
    F _ i -> show i
    S _ i -> show i

data Pattern
  = PVar   Point VName
  | PCtor  Point CName VName
  | PConst Point Constant
  | PWild  Point

instance Show Pattern where
  show = \case
    PVar   _   v -> show v
    PCtor  _ s v -> show s <> " " <> show v
    PConst _   c -> show c
    PWild  _     -> "_"

data Alt = Alt
  { point :: Point
  , pat   :: Pattern
  , body  :: Expr
  }

instance Show Alt where
  show alt = show alt.pat <> " -> " <> show alt.body

data Expr
  = Let    Point [Decl] Expr

  | App    Point Expr Expr
  | Lambda Point [VName] Expr

  | Object Point [(FName, Expr)]
  | Get    Point Expr FName
  | Update Point Expr [(FName, Expr)]

  | Symbol Point CName Expr
  | Case   Point Expr [Alt]

  | Var    Point VName

  | Const  Point Constant

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
    Var    _ v    -> show v
    Const  _ c    -> show c

data Decl = Decl
  { name :: VName
  , sig  :: Scheme
  , body :: Expr
  }

instance Show Decl where
  show d = show d.name <> " : " <> show d.sig <> "\n... = " <> show d.body

data KlassDecl = KlassDecl
  { name   :: TName
  , args   :: [VName]
  , deps   :: [Type]
  , fields :: [(VName, Scheme)]
  }

instance Show KlassDecl where
  show kd = "class " <> show kd.name <> " " <> unwords (map show kd.args) <> " when\n"
    <> indent (punctuate ",\n" (map show kd.deps)) <> "{\n"
    <> indent (unlines (map semi kd.fields))
    <> "}"

data InstanceDecl = InstanceDecl
  { point  :: Point
  , header :: Type
  , deps   :: [Type]
  , impls  :: [(VName, Expr)]
  }

instance Show InstanceDecl where
  show id =
    "instance " <> show id.header <> " when\n"
      <> indent (unlines (map show id.deps))
      <> "{\n"
      <> indent (unlines (map assign id.impls))
      <> "}"

data TopDecl
  = TypeDecls Point [TypeDecl]
  | Decls     Point [Decl]
  | Klass     Point KlassDecl
  | Instance  Point InstanceDecl

instance Show TopDecl where
  show = \case
    TypeDecls _ ds ->
      "mutual {\n"
        <> indent (unlines (map show ds))
        <> "}\n"

    Decls _ ds ->
      "mutual {\n"
        <> indent (unlines (map show ds))
        <> "}\n"

    Klass _ k -> show k
    Instance _ i -> show i

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

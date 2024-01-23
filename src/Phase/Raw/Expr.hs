
module Phase.Raw.Expr where

import Data.Text qualified as Text
import Data.Map qualified as Map

import Name
import Show'
import Ignored

import Phase.Raw.Type
import Data.Function ((&))

data Constant
  = I I Integer
  | F I Double
  | S I Text.Text

instance Show Constant where
  show = \case
    I _ i -> show i
    F _ i -> show i
    S _ i -> show i

-- data Pattern
--   = PCtor I CName VName  -- ctor var

-- instance Show Pattern where
--   show = \case
--     PCtor  _ s v -> show s <> " " <> show v

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
  | Lambda I [VName] Expr

  | Object I      (Map.Map FName Expr)
  | Get    I Expr   FName
  | Update I Expr (Map.Map FName Expr)

  | Symbol I CName Expr
  | Case   I Expr (Map.Map CName (VName, Expr))

  | EVar   I VName

  | Const  I Constant

  | Ann    I Expr Type

  | Refl   I
  | Sym    I Expr
  | Transp I Expr Expr

instance Show' Expr where
  show' p = \case
    Let _ decls k -> "let\n" <> indent (unlines (map show decls)) <> show k
    App _ f x     -> pr p 4 (show' 4 f <> " "    <> show' 5 x)
    Lambda _ xs b -> pr p 3 ("\\" <> punctuate ", " (map show xs) <> " -> " <> show b)
    Object _ fs   -> "{ " <> indent (show' 0 fs) <> " }"
    Get    _ o f  -> show' 3 o <> "." <> show f
    Update _ o fs -> "with " <> show o <> " {\n" <> indent (show' 0 fs) <> "\n}"
    Symbol _ c x  -> show c <> " " <> show' 3 x
    Case   _ o as -> "case " <> show o <> " {\n" <> indent (show' 0 as) <> "}"
    EVar    _ v   -> show v
    Const  _ c    -> show c
    Refl   _      -> "refl"
    Sym    _ e    -> "sym " <> show e
    Transp _ r e  -> "transp " <> show r <> " " <> show e

data Sig = Sig
  { name :: VName
  , sig  :: Rank1
  }

instance Show Sig where
  show d = "sig " <> show d.name <> " : " <> show d.sig

data Decl = Decl
  { i    :: I
  , name :: VName
  , body :: Expr
  }

instance Show Decl where
  show d = "val " <> show d.name <> " = " <> show d.body
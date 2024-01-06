
module Phase.Typed.Expr where

import Data.Text qualified as Text
import Data.Map qualified as Map

import Name
import Show'
import Ignored

import Phase.Typed.Type

import Name
import Ignored

import Data.Function ((&))

data Constant
  = I Integer
  | F Double
  | S Text.Text

instance Show Constant where
  show = \case
    I i -> show i
    F i -> show i
    S i -> show i

data LocalDef
  = LSig  Sig
  | LDecl Decl

instance Show LocalDef where
  show = \case
    LSig  s -> show s
    LDecl s -> show s

data Expr = (:::)
  { expr  :: Expr'
  , type_ :: Type
  }

data Expr'
  = Let    LocalDef Expr

  | App    Expr Expr
  | Lambda VName Expr

  | Object      (Map.Map FName Expr)
  | Get    Expr   FName
  | Update Expr (Map.Map FName Expr)

  | Symbol CName Expr
  | Case   Expr (Map.Map CName (VName, Expr))

  | EVar   VName

  | Const  Constant

  | Ann    Expr Type

instance Show' Expr where
  show' p (e ::: _) = show' p e

instance Show' Expr' where
  show' p = \case
    Let decls k -> "let\n" <> show decls <> show k
    App f x     -> pr p 4 (show' 4 f <> " "    <> show' 5 x)
    Lambda xs b -> pr p 3 ("\\" <> show xs <> " -> " <> show b)
    Object fs   -> "{ " <> indent (show' 0 fs) <> " }"
    Get    o f  -> show' 3 o <> "." <> show f
    Update o fs -> "with " <> show o <> " {\n" <> indent (show' 0 fs) <> "\n}"
    Symbol c x  -> show c <> " " <> show' 3 x
    Case   o as -> "case " <> show o <> " {\n" <> indent (show' 0 as) <> "}"
    EVar    v   -> show v
    Const  c    -> show c

data Sig = Sig
  { name :: VName
  , sig  :: Rank1
  }

instance Show Sig where
  show d = "sig " <> show d.name <> " : " <> show d.sig

data Decl = Decl
  { name :: VName
  , body :: Expr
  }

instance Show Decl where
  show d = "val " <> show d.name <> " = " <> show d.body
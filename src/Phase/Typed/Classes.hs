
module Phase.Typed.Classes where

import Data.Text qualified as Text
import Data.Map qualified as Map

import Name
import Show'
import Ignored

import Phase.Typed.Type
import Phase.Typed.Expr
import Data.Function ((&))
import Pass.TypeCheck.Expr (CanTCExprs)

data KlassDecl = KlassDecl
  { name   :: TName
  , args   :: [TName]
  , deps   :: [Type]
  , fields :: [(VName, Rank1)]
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
  , impls  :: [(VName, Expr)]
  }

instance Show InstanceDecl where
  show id =
    "instance " <> show id.header <> " when\n"
      <> indent (unlines (map show id.deps))
      <> "{\n"
      <> indent (unlines (map assign id.impls))
      <> "}"

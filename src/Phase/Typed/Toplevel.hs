module Phase.Typed.Toplevel where

import Data.Text qualified as Text
import Data.Map qualified as Map

import Name
import Show'
import Ignored

import Phase.Typed.Type
import Phase.Typed.Expr
import Phase.Typed.Classes
import Data.Function ((&))
import Pass.TypeCheck.Expr (CanTCExprs)

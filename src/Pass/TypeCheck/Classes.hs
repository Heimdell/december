
module Pass.TypeCheck.Classes where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Traversable
import Data.Foldable
import Polysemy
import Polysemy.Reader
import Polysemy.Error

import Control.Unification

import Phase.Raw.Type as I
import Phase.Raw.Kind as I
import Phase.Raw.Expr as I
import Phase.Raw.Classes as I

import Phase.Typed.Type as O
import Phase.Typed.Kind as O
import Phase.Typed.Expr as O
import Phase.Typed.Classes as O

import Pass.TypeCheck.Kind
import Pass.TypeCheck.Type
import Pass.TypeCheck.Expr
import Debug.Trace

import Name
import Ignored
import qualified Data.Text as Text
import Control.Monad (unless)

acceptClassDecl
  :: CanTCExprs r
  => I.KlassDecl
  -> (O.KlassDecl -> Sem r a)
  -> Sem r a
acceptClassDecl (I.KlassDecl n as dep fs) k = do
  let telescope :: O.Kind = foldr (O.KArrow . KVar) O.KClass (map (.name) as)
  let ctx0      :: O.Type = foldl (\f x -> O.TArrow f (O.TConst x)) (O.TConst n) (map (.name) as)
  withKinds [(n, telescope)] do
    (dep, fs) <- withKinds (map (\t -> (t.name, KVar t.name)) as) do
      dep <- traverse (checkKindOfType O.KClass) dep

      fs <- for fs \(f, sig) -> do
        Scheme vs (Rank1Base body ctx) <- checkKindOfRank1 sig
        return (f, Scheme vs (Rank1Base body (ctx0 : ctx)))

      return (dep, fs)

    withSchemes fs do
      k (O.KlassDecl n as dep fs)

acceptInstanceDecl
  :: CanTCExprs r
  => I.InstanceDecl
  -> (O.InstanceDecl -> Sem r a)
  -> Sem r a
acceptInstanceDecl (I.InstanceDecl i hdr impls) = do
  error "not supported yet"
module Pass.TypeCheck.Toplevel where

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
import Phase.Raw.Toplevel as I

import Phase.Typed.Type as O
import Phase.Typed.Kind as O
import Phase.Typed.Expr as O
import Phase.Typed.Classes as O

import Pass.TypeCheck.Kind
import Pass.TypeCheck.Type
import Pass.TypeCheck.Expr
import Pass.TypeCheck.Classes
import Debug.Trace

import Name
import Ignored
import qualified Data.Text as Text
import Control.Monad (unless)

{-
data TopDecl
  = ATypeSig  I TypeSig
  | ATypeDecl I TypeDecl
  | ADeclSig  I Sig
  | ADecl     I Decl
  | Klass     I KlassDecl
  | Instance  I InstanceDecl
-}

acceptTopDecl
  :: CanTCExprs r
  => I.TopDecl
  -> Sem r a
  -> Sem r a
acceptTopDecl top k = case top of
  ATypeSig i sig -> do
    withTypeSig sig do
      k

  ATypeDecl i decl -> do
    withTypeDecl decl do
      k

  ADeclSig i sig -> do
    acceptSig sig \_ -> do
      k

  ADecl i decl -> do
    acceptDecl decl \_ -> do
      k

  Klass i klass -> do
    acceptClassDecl klass \_ -> do
      k

  Instance i klass -> do
    acceptInstanceDecl klass \_ -> do
      k

acceptTopDecls
  :: CanTCExprs r
  => [I.TopDecl]
  -> Sem r a
  -> Sem r a
acceptTopDecls [] k = k
acceptTopDecls (d : ds) k = acceptTopDecl d do acceptTopDecls ds k

acceptProg
  :: CanTCExprs r
  => I.Prog
  -> Sem r a
  -> Sem r a
acceptProg prog k = do
  acceptTopDecls prog.decls do
    k
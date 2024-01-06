{-# LANGUAGE OverloadedStrings #-}

module Pass.TypeCheck where

import Polysemy
import Polysemy.Reader
import Polysemy.Error

import Control.Unification

import Phase.Typed.Type
import Phase.Typed.Kind
import Phase.Raw.Toplevel
import Phase.Raw.Kind ()
import Name
import Show'

import Pass.TypeCheck.Classes
import Pass.TypeCheck.Expr
import Pass.TypeCheck.Kind
import Pass.TypeCheck.Type

import Pass.TypeCheck.Toplevel
import Debug.Trace (traceShowM, traceM)
import Data.Traversable (for)
import qualified Data.Map as Map
import Data.Foldable (for_)
import Text.Parser.Yard.Report
import Data.Function ((&))
import Polysemy.State

type R =
  [ Reader Schemes
  , Unifies             Type_ TVar_
  , State (UnifierState Type_ TVar_)
  , Error (Mismatch     Type_ TVar_)
  , Error (Occurs       Type_ TVar_)
  , Refreshes TName
  , Refreshes TVar_
  , Error ExprErr
  , Reader Kinds
  , Reader Structures
  , Unifies             Kind_ KVar_
  , State (UnifierState Kind_ KVar_)
  , Error (Mismatch     Kind_ KVar_)
  , Error (Occurs       Kind_ KVar_)
  , Error KindSigError
  , Error String
  , State Int
  ]

typeCheckProg :: Prog -> Sem R a -> Sem R a
typeCheckProg = acceptProg

typeCheckAndDump :: Sem R ()
typeCheckAndDump = do
  kinds <- ask @Kinds
  for_ (Map.toList kinds) \(n, kind) -> do
    traceM $ show n <> " : " <> show' 0 kind

  structs <- ask @Structures
  for_ (Map.toList structs) \(n, kind) -> do
    traceM $ show n <> " : " <> show' 0 kind

  schemes <- ask @Schemes
  for_ (Map.toList schemes) \(n, kind) -> do
    traceM $ show n <> " : " <> show' 0 kind

reportError
  :: (Show e, Member (Error String) r)
  => Sem (Error e : r) a
  -> Sem r a
reportError = interpretH \case
  Throw e -> throw (show e)

runTC
  :: Sem R a
  -> Map.Map TName Kind
  -> Map.Map VName Rank1
  -> Either String a
runTC ma types values = bar
  where
    bar = ma
      & runReader values
      & runUnification
      & evalState emptyUnifierState
      & reportError
      & reportError
      & runRefreshment
      & runRefreshment
      & reportError
      & runReader types
      & runReader mempty
      & runUnification
      & evalState emptyUnifierState
      & reportError
      & reportError
      & reportError
      & runError
      & evalState 1
      & run

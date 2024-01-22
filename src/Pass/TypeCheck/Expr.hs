{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module Pass.TypeCheck.Expr where

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

import Phase.Typed.Type as O
import Phase.Typed.Kind as O
import Phase.Typed.Expr as O

import Pass.TypeCheck.Kind
import Pass.TypeCheck.Type
import Debug.Trace

import Name
import Ignored
import qualified Data.Text as Text
import Control.Monad
import Text.Parser.Yard.Report
import Show'

type Schemes = Map.Map VName O.Rank1

type CanTCExprs r =
  ( CanTCTypes r
  , Members
      [ Reader Schemes
      , Unifies Type_ TVar_
      , Refreshes TName
      , Refreshes TVar_
      , Error ExprErr
      ] r
  )

data ExprErr
  = IsNotConcreteTypeApplication O.Type I
  | IsNotUnion O.Type I
  | IsNotRecord O.Type I
  | HasNoExposedStructure TName
  | IsNotStructuralType O.Type I
  | FieldSetDiverges O.Type [FName] [FName] I
  | CtorSetDiverges O.Type [CName] [CName] I

instance Show ExprErr where
  show = \case
    IsNotConcreteTypeApplication ty i ->
      rep i do
        "type " <> show ty <> " is not in form 'T a b ...'"

    IsNotUnion ty i -> do
      rep i do
        "type " <> show ty <> " is not a union"

    IsNotRecord ty i -> do
      rep i do
        "type " <> show ty <> " is not a record"

    HasNoExposedStructure tn -> do
      rep tn.name.point do
        "type " <> show tn <> " has no exposed structure"

    IsNotStructuralType ty i -> do
      rep i do
        "type " <> show ty <> " is not structural"

    FieldSetDiverges ty got exp i -> do
      rep i do
        "type " <> show ty <> " expected to have fields " <> show got
          <> " but it has " <> show exp

    CtorSetDiverges ty got exp i -> do
      rep i do
        "type " <> show ty <> " expected to have ctors " <> show got
          <> " but it has " <> show exp


withSchemes :: CanTCExprs r => [(VName, O.Rank1)] -> Sem r a -> Sem r a
withSchemes delta ret = do
  local (Map.fromList delta <>) do
    ret

makeTypeTelescope :: [O.Type] -> O.Type -> O.Type
makeTypeTelescope as r = foldr O.TArrow r as

unApply :: CanTCExprs r => I -> O.Type -> Sem r (TName, [O.Type])
unApply i = \case
  O.TConst n -> return (n, [])
  O.TApp f x -> do
    (f, xs) <- unApply i f
    return (f, xs ++ [x])
  arr@O.TArrow {} ->
    throw (IsNotStructuralType arr i)

  var@Var {} ->
    throw (IsNotConcreteTypeApplication var i)

erase :: CanTCExprs r => I -> O.Type -> Sem r O.Type
erase i ty = do
  ty      <- apply ty
  (f, xs) <- unApply i ty
  xs      <- for xs \_ -> newTVar i "a"
  return (foldl O.TArrow (O.TConst f) xs)

getTypeStructure :: CanTCExprs r => TName -> Sem r O.Rank1TypeExpr
getTypeStructure tname = do
  res <- asks @Structures (Map.lookup tname)
  case res of
    Just it -> return it
    Nothing -> throw (HasNoExposedStructure tname)

ctorsOf :: CanTCExprs r => I -> O.Type -> Sem r (Map.Map CName O.Type)
ctorsOf pt ty = do
  ty                <- apply ty
  (f, xs)           <- unApply pt ty
  Scheme tvs struct <- getTypeStructure f
  case struct of
    O.Record _   -> throw (IsNotUnion ty pt)
    O.Union  map -> return (fmap (substituteTVars (zip tvs xs)) map)

fieldsOf :: CanTCExprs r => I -> O.Type -> Sem r (Map.Map FName O.Type)
fieldsOf pt ty = do
  ty                <- apply ty
  (f, xs)           <- unApply pt ty
  Scheme tvs struct <- getTypeStructure f
  case struct of
    O.Union  _   -> throw (IsNotUnion ty pt)
    O.Record map -> return (fmap (substituteTVars (zip tvs xs)) map)

newTVar :: CanTCExprs r => I -> Text.Text -> Sem r O.Type
newTVar i n = Var <$> refresh (TVar_ (TUName (TName (Name i n 0))))

monotype :: O.Type -> O.Rank1
monotype ty = Scheme [] (Rank1Base ty [])

checkTypeOfExpr
  :: CanTCExprs r
  => O.Type
  -> I.Expr             -- <- raw expr
  -> Sem r
      ( O.Expr          -- -> typed expr
      , Set.Set O.Type  -- -> its requied contexts
      )
checkTypeOfExpr ty = \case
  I.EVar i v -> do
    -- scs <- ask @Schemes
    -- when (not (Map.member v scs)) do
    --   error (show (scs, v))
    -- traceShowM ("expr", scs, v)
    rank1              <- asks @Schemes (Map.! v)  -- get type scheme
    -- traceShowM ("expr", rank1)
    Rank1Base body ctx <- instantiate rank1        -- instantiate unto type
    -- traceShowM ("expr", body, ctx)
    body =:= ty                                    -- check against ideal
    -- traceShowM ("expr", "at")
    return (O.EVar v ::: body, Set.fromList ctx)

  I.App i f x -> do
    xType   <- newTVar i "x"                          -- invent type for arg
    (f, cf) <- checkTypeOfExpr (O.TArrow xType ty) f  -- check that f returns ideal
    (x, cx) <- checkTypeOfExpr           xType     x  -- check that x is fitting
    return (O.App f x ::: ty, cf <> cx)

  I.Lambda i (a : as) b -> do
    xType <- newTVar i "x"                -- invent arg type
    rType <- newTVar i "r"                -- invent res type
    O.TArrow xType rType =:= ty           -- "deconstruct" ty to be (arg -> res)
    withSchemes [(a, monotype xType)] do
      (lam, cl) <- checkTypeOfExpr rType (I.Lambda i as b)
      return (O.Lambda a lam ::: ty, cl)

  I.Lambda i [] b -> do
    checkTypeOfExpr ty b

  I.Object i fs -> do
    fs' <- fieldsOf i ty

    unless (Map.keys fs == Map.keys fs') do
      throw (FieldSetDiverges ty (Map.keys fs) (Map.keys fs') i)

    fs <- for (Map.toList fs) \(f, e) -> do
      (e, ctx) <- checkTypeOfExpr (fs' Map.! f) e
      return ((f, e), ctx)

    let (fs1, ctxs) = unzip fs

    return (O.Object (Map.fromList fs1) ::: ty, mconcat ctxs)

  I.Update i o fs -> do
    -- ty0      <- erase i ty
    (o, ctx) <- checkTypeOfExpr ty o  -- polymorphich updates look scary for now
    fs'      <- fieldsOf i ty

    unless (Map.keysSet fs `Set.isSubsetOf`  Map.keysSet fs') do
      throw (FieldSetDiverges ty (Map.keys fs) (Map.keys fs') i)

    fs <- for (Map.toList fs) \(f, e) -> do
      (e, ctx) <- checkTypeOfExpr (fs' Map.! f) e
      return ((f, e), ctx)

    let (fs1, ctxs) = unzip fs

    return (O.Update o (Map.fromList fs1) ::: ty, ctx <> mconcat ctxs)

  I.Get i o f -> do
    oType    <- newTVar i "o"
    (o, ctx) <- checkTypeOfExpr oType o  -- prevents "{a: 1, b: 2}.a" construct
    fs       <- fieldsOf i o.type_

    case Map.lookup f fs of
      Nothing -> throw (FieldSetDiverges o.type_ [f] (Map.keys fs) i)
      Just ty' -> do
        ty' =:= ty
        return (O.Get o f ::: ty, ctx)

  I.Symbol i c a -> do
    cs <- ctorsOf i ty
    case Map.lookup c cs of
      Nothing -> throw (CtorSetDiverges ty [c] (Map.keys cs) i)
      Just ty -> do
        (a, ctx) <- checkTypeOfExpr ty a
        return (O.Symbol c a ::: ty, ctx)

  I.Case i o as -> do
    oType    <- newTVar i "o"
    (o, ctx) <- checkTypeOfExpr oType o
    cs       <- ctorsOf i oType

    unless (Map.keysSet as == Map.keysSet cs) do
      throw (CtorSetDiverges oType (Map.keys cs) (Map.keys as) i)

    as <- for (Map.toList as) \(c, (a, e)) -> do
      withSchemes [(a, monotype (cs Map.! c))] do
        (e, ctx) <- checkTypeOfExpr ty e
        return ((c, (a, e)), ctx)

    let (as1, ctxs) = unzip as

    return (O.Case o (Map.fromList as1) ::: ty, ctx <> mconcat ctxs)

  I.Let i (d : ds) k -> do
    acceptLocalDecl d \d -> do
      (le, lctx) <- checkTypeOfExpr ty (I.Let i ds k)
      return (O.Let d le ::: ty, lctx)

  I.Let i [] k -> do
    checkTypeOfExpr ty k

  I.Ann i e ty' -> do
    ty' <- checkKindOfType O.KStar ty'
    ty' =:= ty
    checkTypeOfExpr ty' e

  I.Const i c -> do
    (c, ty') <- case c of
      I.I i a -> return (O.I a, O.TConst (TName (Name i "Integer" 0)))
      I.F i a -> return (O.F a, O.TConst (TName (Name i "Float"   0)))
      I.S i a -> return (O.S a, O.TConst (TName (Name i "String"  0)))
    ty' =:= ty
    return (O.Const c ::: ty, mempty)

acceptDecl
  :: CanTCExprs r
  => I.Decl
  -> (O.Decl -> Sem r a)
  -> Sem r a
acceptDecl (I.Decl name body) k = do
  -- scs <- ask @Schemes
  -- when (not (Map.member name scs)) do
  --   error (show (scs, name))
  -- traceShowM (show (scs, name))
  sc <- asks @Schemes (Map.! name)
  -- traceShowM "ok"
  ty0           <- instantiate sc
  -- traceShowM "ka"
  (ty,   ctxs)  <- makeRigid sc
  -- traceShowM "ay"
  (body, ctxs') <- checkTypeOfExpr ty0.body body
  -- traceShowM "y?"
  ty0.body =:= ty
  -- TODO: Solve constraints!
  -- traceShowM ("ENTER", name, sc)
  r <- k (O.Decl name body)
  -- traceShowM ("EXIT ", name, ctxs')
  return r

acceptSig
  :: CanTCExprs r
  => I.Sig
  -> (O.Sig -> Sem r a)
  -> Sem r a
acceptSig (I.Sig name sc) k = do
  sc <- checkKindOfRank1 sc
  -- traceShowM ("SIG", name, sc)
  -- error "TODO: make that tyvars in schemes are Vars, not Consts"
  withSchemes [(name, sc)] do
    k (O.Sig name sc)

acceptLocalDecl
  :: CanTCExprs r
  => I.LocalDef
  -> (O.LocalDef -> Sem r a)
  -> Sem r a
acceptLocalDecl decl k = case decl of
  I.LSig sig -> do
    acceptSig sig (k . O.LSig)

  I.LDecl decl -> do
    acceptDecl decl (k . O.LDecl)

makeRigid :: CanTCExprs r => O.Rank1 -> Sem r (O.Type, [O.Type])
makeRigid (Scheme vs (Rank1Base b ctx)) = do
  rs <- for vs \v -> do
    v' <- refresh v
    return (v, O.TConst v'.name.name)

  let act = substituteTVars rs

  return (act b, map act ctx)

module Pass.TypeCheck.Type where

import Data.Map qualified as Map
import Data.Traversable
import Data.Foldable
import Polysemy
import Polysemy.Reader
import Polysemy.Error

import Control.Unification

import Phase.Raw.Type as I
import Phase.Raw.Kind as I

import Phase.Typed.Type as O
import Phase.Typed.Kind as O

import Pass.TypeCheck.Kind

import Name
import Ignored
import Show'
import Data.Coerce
import Data.Bifunctor
import Debug.Trace (traceShowM)

type Kinds       = Map.Map TName O.Kind
type Implemented = Map.Map TName ()
type Structures  = Map.Map TName O.Rank1TypeExpr

type CanTCTypes r = Members
  [ Reader Kinds
  , Reader Structures
  , Unifies Kind_ KVar_
  , Error KindSigError
  , Refreshes TName
  ] r

data KindSigError
  = ExcessiveTypeVar TName
  | NotEnoughTypeVars I

instance Show KindSigError where
  show = \case
    ExcessiveTypeVar t -> do
      rep t.name.point do
        "excessive type variable"

    NotEnoughTypeVars i -> do
      rep i do
        "not enough typevars"

findType :: CanTCTypes r => TName -> Sem r O.Kind
findType tname = do
  kinds <- ask @Kinds
  case Map.lookup tname kinds of
    Nothing -> error $ "undefined " <> show tname
    Just ty -> return ty

inferKindOfType :: CanTCTypes r => I.Type -> Sem r (O.Type, O.Kind)
inferKindOfType = \case
  I.TConst i tname -> do
    kind <- findType tname
    return (O.TConst tname, kind)

  I.TArrow i d c -> do
    (d', kd) <- inferKindOfType d
    (c', kc) <- inferKindOfType c

    kd =:= O.KStar
    kc =:= O.KStar

    return (O.TArrow d' c', O.KStar)

  I.TApp i f x -> do
    (f', kf) <- inferKindOfType f
    (x', kx) <- inferKindOfType x

    kr <- KVar <$> refresh (TName (Name i "r" 0))

    kf =:= O.KArrow kx kr

    return (O.TApp f' x', kr)

  I.TVar i n -> do
    kind <- findType n.name
    return (Var (TVar_ n), kind)

checkKindOfType :: CanTCTypes r => O.Kind -> I.Type -> Sem r O.Type
checkKindOfType kind ty = do
  (ty', kind') <- inferKindOfType ty
  kind' =:= kind
  return ty'

checkTensor :: CanTCTypes r => Map.Map n I.Type -> Sem r (Map.Map n O.Type)
checkTensor tensor = do
  for tensor (checkKindOfType O.KStar)

withKinds :: CanTCTypes r => [(TName, O.Kind)] -> Sem r a -> Sem r a
withKinds delta = local (Map.fromList delta <>)

checkKindOfRank1 :: CanTCTypes r => I.Rank1 -> Sem r O.Rank1
checkKindOfRank1 rank1 = do
  delta <- for (rank1.typeVars :: [TUName]) \tv -> do
    tv' <- refresh tv.name
    return (tv, KVar tv')

  withKinds (map (first (.name)) delta) do
    body <-           checkKindOfType O.KStar   rank1.body
    ctx  <- traverse (checkKindOfType O.KClass) rank1.ctx
    -- traceShowM ("SCHEME", Rank1Base body ctx)
    -- traceShowM ("SCHEME", generalise $ Rank1Base body ctx)
    return
      $ generalise
      $ Rank1Base body ctx

checkKindOfRank1Class :: CanTCTypes r => I.Rank1 -> Sem r O.Rank1
checkKindOfRank1Class rank1 = do
  delta <- for rank1.typeVars \tv -> do
    tv' <- refresh tv.name
    return (tv', KVar tv')

  withKinds delta do
    body <-           checkKindOfType O.KClass  rank1.body
    ctx  <- traverse (checkKindOfType O.KClass) rank1.ctx
    return
      $ generalise
      $ Rank1Base body ctx

withTypeSig :: CanTCTypes r => I.TypeSig -> Sem r a -> Sem r a
withTypeSig sig ret = do
  withKinds [(sig.name, checkKind sig.kind)] do
    ret

withTypeStruct :: CanTCTypes r => TName -> O.Rank1TypeExpr -> Sem r a -> Sem r a
withTypeStruct tname struct ret = do
  local @Structures (Map.singleton tname struct <>) do
    ret

withTypeVars :: (Coercible t TName, CanTCTypes r) => I -> O.Kind -> [t] -> Sem r a -> Sem r a
withTypeVars _  O.KStar       []       ret = ret
withTypeVars _  O.KClass      []       ret = ret
withTypeVars i (O.KArrow d c) (t : ts) ret = do
  withKinds [(coerce t, d)] do
    withTypeVars i c ts do
      ret
withTypeVars _ _ (t : _) ret = throw (ExcessiveTypeVar (coerce t))
withTypeVars i _  _      ret = throw (NotEnoughTypeVars i)

withTypeDecl :: CanTCTypes r => I.TypeDecl -> Sem r a -> Sem r a
withTypeDecl decl ret = do
  kind   <- findType decl.name
  struct <- withTypeVars decl.point kind decl.typeVars do
    case decl.body of
      I.Union  i tensor -> O.Union  <$> checkTensor tensor
      I.Record i tensor -> O.Record <$> checkTensor tensor

  withTypeStruct decl.name (Scheme (map TVar_ decl.typeVars) struct) do
    ret
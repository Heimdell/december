{-# LANGUAGE OverloadedStrings #-}

module Pass.TypeCheck where

import Data.Map qualified as Map
import Data.Traversable
import Data.Function ((&))
import Data.Bifunctor
import Polysemy
import Polysemy.Reader
import Polysemy.Error

import Text.Parser.Yard.Report
import Text.Parser.Yard.Point
import Control.Unification

import Phase.Raw as I
import Phase.Typed.Type as O
import Phase.Typed.Kind as O
import Name
import Ignored

data Context = Context
  { types  :: Map.Map Name O.Kind
  , struct :: Map.Map Name (Maybe (Scheme O.TypeExpr_ TVar_))
  , values :: Map.Map Name (Scheme O.Type_ TVar_)
  }

data TypeArgsError
  = TooMuchTypeArgs Name
  | NotEnoughTypeArgs Point

type Row =
  [ Unifies O.Type_ TVar_
  , Unifies O.Kind_ KVar_
  , Refreshes TVar_
  , Refreshes KVar_
  , Reader Context
  , Error TypeArgsError
  ]
type CanTC r = Members Row r

-- runScopeCheck :: Sem Row a -> Context -> Either TCError a
-- runScopeCheck ma ctx = ma
--     & runError @TCError
--     & runReader @Context ctx
--     & run

data Lens s a = Lens
  { get    :: s -> a
  , modify :: (a -> a) -> (s -> s)
  }

findType :: CanTC r => Name -> Sem r O.Kind
findType n = do
  asks @Context (Map.lookup n . (.types)) >>= \case
    Just k -> return k
    Nothing -> error $ "type " <> show n <> " is unknown\n" <> show (Report n.point.raw "here")

traverse1
  :: CanTC r
  => ( a  -> Sem r b -> Sem r b)
  -> ([a] -> Sem r b -> Sem r b)
traverse1 f xs k = foldr f k xs

withType :: CanTC r => (Name, O.Kind) -> Sem r a -> Sem r a
withType (n, k) ret = do
  local
    (\ctx -> ctx
      { types = Map.insert n k ctx.types
      }
    )
    ret

withTypeStructure :: CanTC r => (Name, Scheme O.TypeExpr_ TVar_) -> Sem r a -> Sem r a
withTypeStructure (n, k) ret = do
  local
    (\ctx -> ctx
      { struct = Map.insert n (Just k) ctx.struct
      }
    )
    ret

inferKindOfType :: CanTC r => I.Type -> Sem r (O.Kind, O.Type)
inferKindOfType = \case
  Var tvar -> do
    k <- findType tvar
    return (k, Var (TVar_ tvar))

  I.TArrow i d c -> do
    (kd, d) <- inferKindOfType d
    (kc, c) <- inferKindOfType c

    kd =:= O.KStar i
    kc =:= O.KStar i

    return (O.KStar i, O.TArrow i d c)

  I.TApp i f x -> do
    (kf, f) <- inferKindOfType f
    (kx, x) <- inferKindOfType x

    kr <- refresh (KVar_ (Name (Ign i) "r" 0))
    kf =:= O.KArrow i kx (Var kr)

    return (Var kr, O.TApp i f x)

inferKindOfRank1 :: CanTC r => I.Rank1 -> Sem r (O.Kind, Scheme (Term O.Type_) O.TVar_)
inferKindOfRank1 scm = do
  kinds <- for scm.typeVars \n -> do
    k <- refresh (KVar_ n)
    return (n, Var k)

  traverse1 withType kinds do
    (k, body) <- inferKindOfType scm.body

    return (k, Scheme (map TVar_ scm.typeVars) body)

checkKindOfExpr :: CanTC r => O.Kind -> I.Type -> Sem r O.Type
checkKindOfExpr k t = do
  (k', t') <- inferKindOfType t
  k =:= k
  return t'

checkKindOfTypeExpr :: CanTC r => I.TypeExpr -> Sem r O.TypeExpr
checkKindOfTypeExpr = \case
  I.Union  i cs -> O.Union  i <$> (traverse . traverse) (checkKindOfExpr (O.KStar i.raw)) cs
  I.Record i cs -> O.Record i <$> (traverse . traverse) (checkKindOfExpr (O.KStar i.raw)) cs

addTypeSignature :: CanTC r => Name -> O.Kind -> Sem r a -> Sem r a
addTypeSignature n k = withType (n, k)

bindTypeVars :: CanTC r => [Name] -> O.Kind -> Sem r a -> Sem r a
bindTypeVars ns k ret =
  case (ns, k) of
    (n : ns, O.KArrow i k l) -> do
      withType (n, k) do
        bindTypeVars ns l ret

    ([], O.KStar i) -> do
      ret

    (n : ns, O.KStar i) -> do
      throw (TooMuchTypeArgs n)

    ([], O.KArrow i _ _) -> do
      throw (NotEnoughTypeArgs i)

checkTypeDecl :: CanTC r => I.TypeDecl -> Sem r a -> Sem r a
checkTypeDecl decl ret = do
  k <- findType decl.name
  bindTypeVars decl.kindVars k do
    te <- checkKindOfTypeExpr decl.body
    withTypeStructure (decl.name, Scheme (map TVar_ decl.kindVars) te) do
      ret
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Pass.ScopeCheck where

import Data.Coerce
import Data.Map qualified as Map
import Data.Traversable
import Data.Function ((&))
import Data.Bifunctor
import Polysemy
import Polysemy.Reader
import Polysemy.Error

import Text.Parser.Yard.Report

import Phase.Raw
import Name
import Ignored

data Context = Context
  { types  :: Map.Map Name Name
  , values :: Map.Map Name Name
  }

data SCError
  = Undefined Name

instance Show SCError where
  show (Undefined n) =
    "name " <> show n <> " is undefined"

type CanSC r = Members [Error SCError, Reader Context] r

runScopeCheck :: Sem [Error SCError, Reader Context] a -> Context -> Either SCError a
runScopeCheck ma ctx = ma
    & runError @SCError
    & runReader @Context ctx
    & run

data Lens s a = Lens
  { get    :: s -> a
  , modify :: (a -> a) -> (s -> s)
  }

type Selector = Lens Context (Map.Map Name Name)

typesL :: Selector
typesL = Lens
  { get    = (.types)
  , modify = \f ctx -> ctx { types = f ctx.types }
  }

valuesL :: Selector
valuesL = Lens
  { get    = (.values)
  , modify = \f ctx -> ctx { values = f ctx.values }
  }

traverse'
  :: CanSC r
  => ( a  -> ( a  -> Sem r b) -> Sem r b)
  -> ([a] -> ([a] -> Sem r b) -> Sem r b)
traverse' f xs ret =
  case xs of
    [] -> ret []
    x : xs -> do
      f x \x -> do
        traverse' f xs \xs -> do
          ret (x : xs)

renew :: Selector -> Name -> Context -> (Context, Name)
renew lens name ctx =
  case Map.lookup name $ lens.get ctx of
    Nothing ->
      let name'' = updateName name in
      ( lens.modify (Map.insert name name'') ctx
      , name''
      )

    Just name' ->
      let name'' = updateName name' in
      ( lens.modify (Map.insert name name'') ctx
      , name''
      )

withName :: (Coercible n Name, CanSC r) => Selector -> n -> (n -> Sem r a) -> Sem r a
withName lens n ret = do
  ctx <- ask
  let (ctx', name') = renew lens (coerce n) ctx
  local (const ctx') do
    ret (coerce name')

withNames :: (Coercible n Name, CanSC r) => Selector -> [n] -> ([n] -> Sem r a) -> Sem r a
withNames lens = traverse' (withName lens)

checkName :: (Coercible n Name, CanSC r) => Selector -> n -> Sem r n
checkName lens name = do
  renamer <- asks lens.get
  case Map.lookup (coerce name) renamer of
    Just found -> return (coerce found)
    Nothing    -> throw (Undefined (coerce name))

checkType :: CanSC r => Type -> Sem r Type
checkType = \case
  TConst i n -> do
    n <- checkName typesL n
    return (TConst i n)

  TArrow i d c -> do
    d <- checkType d
    c <- checkType c
    return (TArrow i d c)

  TApp i f x -> do
    f <- checkType f
    x <- checkType x
    return (TApp i f x)

  TVar i n -> do
    n <- checkName typesL n
    return (TVar i n)

checkRank1 :: CanSC r => Rank1 -> Sem r Rank1
checkRank1 rank1 = do
  withNames typesL rank1.typeVars \tvars -> do
    body <-          checkType rank1.body
    ctx  <- traverse checkType rank1.ctx
    return rank1
      { typeVars = tvars
      , body     = body
      , ctx      = ctx
      }

checkTypeExpr :: CanSC r => TypeExpr -> Sem r TypeExpr
checkTypeExpr = \case
  Union  i fs -> Union  i <$> traverse checkType fs
  Record i fs -> Record i <$> traverse checkType fs

checkTypeSig :: CanSC r => TypeSig -> (TypeSig -> Sem r a) -> Sem r a
checkTypeSig tsig ret = do
  withName typesL tsig.name \name -> do
    ret tsig {name}

checkTypeDecl :: CanSC r => TypeDecl -> Sem r TypeDecl
checkTypeDecl tdecl = do
  name <- checkName typesL tdecl.name
  withNames typesL tdecl.typeVars \typeVars -> do
    body <- checkTypeExpr tdecl.body
    return tdecl
      { name
      , typeVars
      , body
      }

checkAlt :: CanSC r => (CName, (VName, Expr)) -> Sem r (CName, (VName, Expr))
checkAlt (ctor, (var, expr)) = do
  withName valuesL var \var -> do
    expr <- checkExpr expr
    return (ctor, (var, expr))

checkSig :: CanSC r => Sig -> (Sig -> Sem r a) -> Sem r a
checkSig sign ret = do
  sig <- checkRank1 sign.sig
  withName valuesL sign.name \name -> do
    ret sign {name, sig}

checkDecl :: CanSC r => Decl -> Sem r Decl
checkDecl decl = do
  name <- checkName valuesL decl.name
  body <- checkExpr decl.body
  return (decl {name = name, body} :: Decl)

checkLocalDef :: CanSC r => LocalDef -> (LocalDef -> Sem r a) -> Sem r a
checkLocalDef localDef ret = do
  case localDef of
    LDecl decl -> do
      decl <- checkDecl decl
      ret (LDecl decl)

    LSig sig -> do
      checkSig sig \sig -> do
        ret (LSig sig)

checkExpr :: CanSC r => Expr -> Sem r Expr
checkExpr = \case
  Let i decls body -> do
    traverse' checkLocalDef decls \decls -> do
      body <- checkExpr body
      return (Let i decls body)

  App i f x -> do
    f <- checkExpr f
    x <- checkExpr x
    return (App i f x)

  Lambda i args body -> do
    withNames valuesL args \args -> do
      body <- checkExpr body
      return (Lambda i args body)

  Object i fs -> do
    fs <- for (Map.toList fs) \(f, e) -> do
      e <- checkExpr e
      return (f, e)

    return (Object i (Map.fromList fs))

  Get i o f -> do
    o <- checkExpr o
    return (Get i o f)

  Update i o fs -> do
    o <- checkExpr o
    fs <- for (Map.toList fs) \(f, e) -> do
      e <- checkExpr e
      return (f, e)

    return (Update i o (Map.fromList fs))

  Symbol i c x -> do
    x <- checkExpr x
    return (Symbol i c x)

  Case i o as -> do
    o  <-          checkExpr o
    as <- traverse checkAlt  (Map.toList as)
    return (Case i o (Map.fromList as))

  EVar i v -> do
    v <- checkName valuesL v
    return (EVar i v)

  Const i c -> do
    return (Const i c)

checkKlassDecl :: CanSC r => KlassDecl -> (KlassDecl -> Sem r a) -> Sem r a
checkKlassDecl klass ret = do
  withName typesL klass.name \name -> do
    (deps, fields, args) <-
      withNames typesL klass.args \args -> do
        deps <- traverse checkType klass.deps
        fields <- for klass.fields \(f, t) -> do
          t <- checkRank1 t
          return (f, t)
        return (deps, fields, args)

    withNames valuesL (map fst klass.fields) \_ -> do
      ret klass {name, args, deps, fields}

checkInstanceDecl :: CanSC r => InstanceDecl -> Sem r InstanceDecl
checkInstanceDecl inst = do
  header <-          checkRank1 inst.header
  -- deps   <- traverse checkType  inst.deps
  impls  <- for inst.impls \(f, e) -> do
    e <- checkExpr e
    return (f, e)

  return inst {header, impls}

checkTopDecl :: CanSC r => TopDecl -> (TopDecl -> Sem r a) -> Sem r a
checkTopDecl decl ret =
  case decl of
    ATypeSig  i x -> checkTypeSig   x \x -> ret (ATypeSig i x)
    ADeclSig  i x -> checkSig       x \x -> ret (ADeclSig i x)
    Klass     i x -> checkKlassDecl x \x -> ret (Klass    i x)
    ATypeDecl i x -> do x <- checkTypeDecl     x; ret (ATypeDecl i x)
    ADecl     i x -> do x <- checkDecl         x; ret (ADecl     i x)
    Instance  i x -> do x <- checkInstanceDecl x; ret (Instance  i x)

checkImported :: CanSC r => Imported -> (Imported -> Sem r a) -> Sem r a
checkImported imp ret =
  case imp of
    Typename t -> withName typesL  t \t -> ret (Typename t)
    Value    v -> withName valuesL v \v -> ret (Value    v)

checkImport :: CanSC r => Import -> (Import -> Sem r a) -> Sem r a
checkImport imp ret = do
  traverse' checkImported imp.items \items -> do
    ret imp {items}

checkProg :: CanSC r => Prog -> Sem r Prog
checkProg prog = do
  traverse' checkImport prog.imports \imports -> do
    traverse' checkTopDecl prog.decls \decls -> do
      return prog {imports, decls}

equip :: SCError -> Report SCError
equip (Undefined name) = Report name.point.raw (Undefined name)

scopeCheck :: Context -> Prog -> Either (Report SCError) Prog
scopeCheck ctx prog = first equip $ runScopeCheck (checkProg prog) ctx
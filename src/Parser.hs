
module Parser where

import Data.Text qualified as Text
import Data.Maybe

import Text.Parser.Yard hiding (Expr)

import AST

isReserved w = Text.unpack w ` elem` words
  "let in type import mutual do using val case of record class instance when where with"

space :: Parser ()
space = space1
  <|> lineComment "--"
  <|> blockComment "{-" "-}"

token :: Parser a -> Parser a
token = mkToken (void (many space))

slug :: String -> Parser ()
slug = token . try . string

vname :: Parser VName
tname :: Parser TName
cname :: Parser CName
fname :: Parser FName
mname :: Parser MName
vname = VName <$> token do           ctor Name <*> kebabLowerCase isReserved
tname = TName <$> token do           ctor Name <*> kebabUpperCase isReserved
mname = MName <$> token do           ctor Name <*> kebabUpperCase isReserved
cname = CName <$> token do char '#'; ctor Name <*> kebabUpperCase isReserved
fname = FName <$> token do           ctor Name <*> kebabLowerCase isReserved

imported :: Parser Imported
imported = (Typename <$> tname) <|> (Value <$> vname)

import_ :: Parser Import
import_ = do
  ctor Import
    <*  slug "import"
    <*> mname
    <*  slug "using"
    <*  slug "("
    <*> (imported `sepBy` slug ",")
    <*  slug ")"

group :: Parser a -> Parser a
group p = slug "(" *> p <* slug ")"

kind :: Parser Kind
kind = do
  p <- getPosition
  d <- kindTerm
  c <- optional do
    slug "=>"
    kind
  return (foldr (flip (KArrow p)) d c)
  where
    kindTerm
      =   ctor KStar <*  slug "*"
      <|> ctor KVar  <*> vname
      <|> group kind

type_ :: Parser Type
type_ = do
  p <- getPosition
  d <- typeApp
  c <- optional do
    slug "->"
    type_
  return (foldr (flip (TArrow p)) d c)
  where
    typeApp :: Parser Type
    typeApp = do
      p  <- getPosition
      xs <- some typeTerm
      return (foldl1 (TApp p) xs)
      where
        typeTerm = choose
          [ ctor TVar   <*> vname
          , ctor TConst <*> tname
          , group type_
          ]

option :: Alternative f => a -> f a -> f a
option a ma = fromMaybe a <$> optional ma

scheme :: Parser Scheme
scheme = polytype <|> monotype
  where
    monotype = ctor Scheme <*> pure [] <*> type_ <*> pure []
    polytype =
      ctor Scheme
        <*  slug "type"
        <*> some vname
        <*  slug "."
        <*> type_
        <*> option [] do
              slug "when"
              type_ `sepBy1` slug ","

typeExpr :: Parser TypeExpr
typeExpr = record <|> union
  where
    record =
      ctor Record
        <*  slug "{"
        <*> do fieldDecl `sepBy` slug ","
        <*  slug "}"

    union =
      ctor Union
        <*  slug "<"
        <*> do ctorDecl  `sepBy` slug ","
        <*  slug ">"

    fieldDecl = do
      f <- fname
      _ <- slug ":"
      t <- type_
      return (f, t)

    ctorDecl = do
      c <- cname
      _ <- slug ":"
      t <- type_
      return (c, t)

kvar :: Parser (VName, Kind)
kvar = group do
  v <- vname
  _ <- slug ":"
  k <- kind
  return (v, k)

typeDecl :: Parser TypeDecl
typeDecl = do
  pure TypeDecl
    <*  slug "type"
    <*> tname
    <*> many kvar
    <*  slug "="
    <*> typeExpr

constant :: Parser Constant
constant = token do
      ctor (\p -> either (F p) (I p)) <*> number
  <|> ctor S <* char '\'' <*> stringLiteral defaultCharEscapes "\'" <* char '\''

pattern_ :: Parser Pattern
pattern_ = choose
  [ ctor PCtor  <*> cname <*> vname
  , ctor PVar   <*> vname
  , ctor PConst <*> constant
  , ctor PWild  <*  slug "_"
  ]

alt :: Parser Alt
alt = do
  ctor Alt
    <*> pattern_
    <*  slug "->"
    <*> expr

fieldAssign :: Parser (FName, Expr)
fieldAssign = do
  pure (,) <*> fname <* slug "=" <*> expr

expr :: Parser Expr
expr = letExpr <|> appExpr
  where
    letExpr = do
      ctor Let
        <*  slug "let"
        <*> do decl `sepBy1` slug ","
        <*  slug ";"
        <*> expr

    appExpr = do
      p   <- getPosition
      fxs <- some getExpr
      return (foldl1 (App p) fxs)

    getExpr = do
      p <- getPosition
      t <- termExpr
      fs <- many do
        slug "."
        fname
      return (foldl (Get p) t fs)

    termExpr = choose
      [ ctor Lambda
          <*     slug "\\"
          <*> do vname `sepBy1` slug ","
          <*     slug "->"
          <*>    expr

      , ctor Object
          <*     slug "{"
          <*> do fieldAssign `sepBy` slug ","
          <*     slug "}"

      , ctor Update
          <*     slug "with"
          <*>    expr
          <*     slug "do"
          <*     slug "{"
          <*> do fieldAssign `sepBy` slug ","
          <*     slug "}"

      , ctor Symbol
          <*> cname
          <*> termExpr

      , ctor Case
          <*  slug "case"
          <*> expr
          <*  slug "of"
          <*  slug "{"
          <*> do alt `sepBy` slug ";"
          <*  slug "}"

      , ctor Var
          <*> vname

      , ctor Const
          <*> constant

      , group expr
      ]

decl :: Parser Decl
decl = do
  pure Decl
    <*  slug "val"
    <*> vname
    <*  slug ":"
    <*> scheme
    <*  optional (slug "...")
    <*  slug "="
    <*> expr

klassDecl :: Parser KlassDecl
klassDecl = do
  pure KlassDecl
    <*  slug "class"
    <*> tname
    <*> some vname
    <*> option [] do
      slug "when"
      do type_ `sepBy` slug ","
    <*  slug "{"
    <*> flip sepBy (slug ";") do
      pure (,)
        <*> vname
        <*  slug ":"
        <*> scheme
    <*  slug "}"

instanceDecl :: Parser InstanceDecl
instanceDecl = do
  ctor InstanceDecl
    <*  slug "instance"
    <*> type_
    <*> option [] do
      slug "when"
      do type_ `sepBy` slug ","
    <*  slug "{"
    <*> flip sepBy (slug ";") do
      pure (,)
        <*> vname
        <*  slug "="
        <*> expr
    <*  slug "}"

topDecl :: Parser TopDecl
topDecl = mutual <|> klass <|> instonce <|> tdecl <|> vdecl
  where
    mutual = do
      slug "mutual"
      slug "{"
      res <- (ctor TypeDecls <*> some typeDecl) <|> (ctor Decls <*> some decl)
      slug "}"
      return res

    klass    = ctor Klass     <*> klassDecl
    instonce = ctor Instance  <*> instanceDecl
    tdecl    = ctor TypeDecls <*> (pure <$> typeDecl)
    vdecl    = ctor Decls     <*> (pure <$> decl)

prog :: Parser Prog
prog =
  pure Prog
    <*  many space
    <*  slug "module"
    <*> mname
    <*  slug "where"
    <*> many import_
    <*> many topDecl
    <*  eof
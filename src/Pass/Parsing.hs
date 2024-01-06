
module Pass.Parsing where

import Data.Text qualified as Text
import Data.Maybe

import Text.Parser.Yard hiding (Expr)
import Control.Unification

import Phase.Raw
import Name
import Ignored

ctor' :: (Ign Point -> c) -> Parser c
ctor' f = ctor (f . Ign)

isReserved w = Text.unpack w ` elem` words
  "let in type kind sig import mutual do using val case of record class instance when where with"

space :: Parser ()
space = space1
  <|> lineComment "--"
  <|> blockComment "{-" "-}"

token :: Parser a -> Parser a
token = mkToken (void (many space))

slug :: String -> Parser ()
slug = token . try . string

lname :: Parser Name
lname = token do ctor' Name <*> kebabLowerCase isReserved <*> pure 0

uname :: Parser Name
uname = token do ctor' Name <*> kebabUpperCase isReserved <*> pure 0

vname :: Parser Name
tname :: Parser Name
cname :: Parser Name
fname :: Parser Name
mname :: Parser Name
vname = token    lname
tname = token    uname
mname = token    uname
cname = token do char '#'; uname
fname = token    lname

imported :: Parser Imported
imported = (Typename <$> tname) <|> (Value <$> vname)

import_ :: Parser Import
import_ = do
  ctor' Import
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
  p <- Ign <$> getPosition
  d <- kindTerm
  c <- optional do
    slug "=>"
    kind
  return (foldr (flip (KArrow p)) d c)
  where
    kindTerm
      =   ctor' KStar <*  slug "*"
      <|> group kind

type_ :: Parser Type
type_ = do
  p <- Ign <$> getPosition
  d <- typeApp
  c <- optional do
    slug "->"
    type_
  return (foldr (flip (TArrow p)) d c)
  where
    typeApp :: Parser Type
    typeApp = do
      p  <- Ign <$> getPosition
      xs <- some typeTerm
      return (foldl1 (TApp p) xs)
      where
        typeTerm = choose
          [ TConst <$> (Ign <$> getPosition) <*> lname
          , TConst <$> (Ign <$> getPosition) <*> uname
          , group type_
          ]

option :: Alternative f => a -> f a -> f a
option a ma = fromMaybe a <$> optional ma

scheme :: Parser Rank1
scheme = polytype <|> monotype
  where
    monotype = ctor' Rank1 <*> pure [] <*> type_ <*> pure []
    polytype =
      ctor' Rank1
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
      ctor' Record
        <*  slug "{"
        <*> do fieldDecl `sepBy` slug ","
        <*  slug "}"

    union =
      ctor' Union
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

kvar :: Parser (Name, Kind)
kvar = group do
  v <- vname
  _ <- slug ":"
  k <- kind
  return (v, k)

typeSig :: Parser TypeSig
typeSig = do
  pure TypeSig
    <*  slug "kind"
    <*> tname
    <*  slug ":"
    <*> kind

typeDecl :: Parser TypeDecl
typeDecl = do
  pure TypeDecl
    <*  slug "type"
    <*> tname
    <*> many vname
    <*  slug "="
    <*> typeExpr

constant :: Parser Constant
constant = token do
      ctor' (\p -> either (F p) (I p)) <*> number
  <|> ctor' S <* char '\'' <*> stringLiteral defaultCharEscapes "\'" <* char '\''

pattern_ :: Parser Pattern
pattern_ = choose
  [ ctor' PCtor  <*> cname <*> vname
  ]

alt :: Parser Alt
alt = do
  ctor' Alt
    <*> pattern_
    <*  slug "->"
    <*> expr

fieldAssign :: Parser (Name, Expr)
fieldAssign = do
  pure (,) <*> fname <* slug "=" <*> expr

expr :: Parser Expr
expr = letExpr <|> appExpr
  where
    letExpr = do
      ctor' Let
        <*  slug "let"
        <*> do localDef `sepBy1` slug ","
        <*  slug ";"
        <*> expr

    appExpr = do
      p   <- Ign <$> getPosition
      fxs <- some getExpr
      return (foldl1 (App p) fxs)

    getExpr = do
      p <- Ign <$> getPosition
      t <- termExpr
      fs <- many do
        slug "."
        fname
      return (foldl (Get p) t fs)

    termExpr = choose
      [ ctor' Lambda
          <*     slug "\\"
          <*> do vname `sepBy1` slug ","
          <*     slug "->"
          <*>    expr

      , ctor' Object
          <*     slug "{"
          <*> do fieldAssign `sepBy` slug ","
          <*     slug "}"

      , ctor' Update
          <*     slug "with"
          <*>    expr
          <*     slug "do"
          <*     slug "{"
          <*> do fieldAssign `sepBy` slug ","
          <*     slug "}"

      , ctor' Symbol
          <*> cname
          <*> termExpr

      , ctor' Case
          <*  slug "case"
          <*> expr
          <*  slug "of"
          <*  slug "{"
          <*> do alt `sepBy` slug ";"
          <*  slug "}"

      , ctor' EVar
          <*> vname

      , ctor' Const
          <*> constant

      , group expr
      ]

localDef :: Parser LocalDef
localDef = (LSig <$> sig) <|> (LDecl <$> decl)

sig :: Parser Sig
sig = do
  pure Sig
    <*  slug "sig"
    <*> vname
    <*  slug ":"
    <*> scheme

decl :: Parser Decl
decl = do
  pure Decl
    <*  slug "val"
    <*> vname
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
  ctor' InstanceDecl
    <*  slug "instance"
    <*> scheme
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
topDecl = choose
  [ ctor' Klass     <*> klassDecl
  , ctor' Instance  <*> instanceDecl
  , ctor' ATypeDecl <*> typeDecl
  , ctor' ATypeSig  <*> typeSig
  , ctor' ADecl     <*> decl
  , ctor' ADeclSig  <*> sig
  ]

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
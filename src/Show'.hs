
module Show' where

import Data.Map qualified as Map
import Control.Unification
import Data.Function ((&))

import Ignored
import Text.Parser.Yard.Report

class Show' a where
  show' :: Int -> a -> String

instance {-# OVERLAPPABLE #-} Show' a => Show a where
  show = show' 0

instance (Show' (t (Term t v)), Show v) => Show' (Term t v) where
  show' p = \case
    Var    v -> show    v
    Struct s -> show' p s

instance (Show' (t v), Show v) => Show' (Scheme t v) where
  show' _ (Scheme vs t) =
    "type " <> punctuate ", " (map show vs) <> ". " <> show t

instance (Show' a, Show' b) => Show' (Map.Map a b) where
  show' _ m = m
    & Map.toList
    & map semi
    & punctuate ", "

instance (Show' a, Show' b) => Show' (a, b) where
  show' _ (a, b) = show' 0 a <> " -> " <> show' 0 b

pr :: Int -> Int -> String -> String
pr p n str = if p > n then "(" <> str <> ")" else str

punctuate :: String -> [String] -> String
punctuate sep []       = ""
punctuate sep [x]      = x
punctuate sep [x, y]   = x <> sep <> y
punctuate sep (x : xs) = x <> sep <> punctuate sep xs

indent :: String -> String
indent = unlines . map ("  " ++) . lines

semi (v, k) = show v <> ": " <> show k

assign (v, k) = show v <> " = " <> show k

newtype Unquote = Unquote String

instance Show Unquote where
  show (Unquote s) = s


rep :: I -> String -> String
rep i s =
  show do
    Report i.raw do
      Unquote do
        s

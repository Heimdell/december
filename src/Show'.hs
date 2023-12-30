
module Show' where

class Show' a where
  show' :: Int -> a -> String

instance {-# OVERLAPPABLE #-} Show' a => Show a where
  show = show' 0

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
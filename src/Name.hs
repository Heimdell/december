
module Name where

import Data.Text qualified as Text

import Text.Parser.Yard.Point
import Ignored

data Name = Name
  { point :: Ign Point
  , raw   :: Text.Text
  , index :: Int
  }
  deriving stock (Eq, Ord)

instance Show Name where
  show n = Text.unpack n.raw <> if n.index /= 0 then "'" <> show n.index else ""

updateName :: Name -> Name
updateName n = n { index = 1 + n.index }
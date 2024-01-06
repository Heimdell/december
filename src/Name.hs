
module Name where

import Data.Text qualified as Text
import Data.String

import Text.Parser.Yard.Point
import Ignored
import Show'
import Control.Unification

data Name = Name
  { point :: I
  , raw   :: Text.Text
  , index :: Int
  }
  deriving stock (Eq, Ord)

instance Show' Name where
  show' _ n = Text.unpack n.raw <> if n.index /= 0 then "'" <> show n.index else ""

updateName :: Name -> Name
updateName n = n { index = 1 + n.index }

newtype VName = VName { name :: Name } deriving newtype (Eq, Ord, Show')
newtype TName = TName { name :: Name } deriving newtype (Eq, Ord, Show', IsName)
newtype CName = CName { name :: Name } deriving newtype (Eq, Ord)
newtype FName = FName { name :: Name } deriving newtype (Eq, Ord, Show')
newtype MName = MName { name :: Name } deriving newtype (Eq, Ord, Show')

newtype TUName = TUName { name :: TName } deriving newtype (Eq, Ord)

instance Show' CName where
  show' _ n = "#" <> show n.name

instance Show' TUName where
  show' _ n = "'" <> show n.name

instance IsName Name where
  setNameIndex i (Name p n _) = Name p n i

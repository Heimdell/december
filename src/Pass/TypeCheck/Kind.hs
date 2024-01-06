
module Pass.TypeCheck.Kind where

import Phase.Raw.Kind as I
import Phase.Typed.Kind as O

checkKind :: I.Kind -> O.Kind
checkKind = \case
  I.KStar  i     -> O.KStar
  I.KArrow i d c -> O.KArrow (checkKind d) (checkKind c)
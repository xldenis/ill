module Thrill.Syntax.Pattern where

import Thrill.Prelude

import Thrill.Syntax.Pretty
import Thrill.Syntax.Literal
import Thrill.Syntax.Name

import           Control.Comonad.Cofree

data Pattern nm a
  = Destructor nm [a]
  | Wildcard
  | PVar nm
  | PLit Literal
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic1)

type Pat' nm a = Cofree (Pattern nm) a

type Pat a = Pat' QualifiedName a
type Patterns nm a = [Pat' nm a]

instance Eq nm => Eq1 (Pattern nm) where
  liftEq = liftEqDefault

instance Show nm => Show1 (Pattern nm) where
  liftShowsPrec = liftShowsPrecDefault

instance Pretty1 f => Pretty (Cofree f a) where
  pretty (a :< el) = liftPretty pretty el

instance Pretty nm => Pretty1 (Pattern nm) where
  liftPretty pretty' (Destructor cons args) = pretty cons <-> hsep (map
    (\a -> let
      doc = pretty' a
    in parensIf (complexDoc doc) doc) args)
  liftPretty pretty' (Wildcard) = pretty "_"
  liftPretty pretty' (PVar x) = pretty x
  liftPretty pretty' (PLit l) = pretty l

patternNames :: Pat' nm a -> [nm]
patternNames (_ :< PVar n) = [n]
patternNames (_ :< Destructor _ pats) = concatMap patternNames pats
patternNames (_ :< Wildcard) = []

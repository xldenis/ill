module Ill.Syntax.Pattern where
import Ill.Syntax.Pretty
import Ill.Inference.Type


data Pattern
  = Destructor String [Pattern]
  | Wildcard
  | PVar String
  -- | PLit Literal
  deriving (Eq, Show)

instance Pretty Pattern where
  pretty (Destructor cons args) = text cons <+> hsep (map (\a -> parensIf (complex a) (pretty a)) args)
    where complex (Destructor _ _) = True
          complex _ = False
  pretty Wildcard = text "_"
  pretty (PVar x) = text x

tiPat :: Pattern -> TI ([Pred], [Assump], Type)

tiPat (PVar i) = do v <- newTVar Star
                    return ([], [i :>: toScheme v], v)
tiPat Wildcard   = do v <- newTVar Star
                      return ([], [], v)


-- tiPat (PAs i pat) = do (ps, as, t) <- tiPat pat
                       -- return (ps, (i:>:toScheme t):as, t)
-- tiPat (PLit l) = do (ps, t) <- ST.tiLit l
                    -- return (ps, [], t)
-- tiPat (PCon (i:>:sc) pats) = do (ps,as,ts) <- tiPats pats
                                -- t'         <- newTVar Star
                                -- (qs :=> t) <- freshInst sc
                                -- unify t (foldr fn t' ts)
                                -- return (ps++qs, as, t')

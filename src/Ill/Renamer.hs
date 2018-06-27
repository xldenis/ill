module Ill.Renamer where

import           Ill.Syntax

import           Ill.Error
import           Ill.Syntax.Name        as N
import           Ill.Syntax.Pretty

import           Control.Lens           hiding ((:<))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor         (first, second)
import           Debug.Trace
import           Ill.BindingGroup
import           Ill.Prelude
import qualified Ill.Syntax.Builtins    as Builtins

import           Control.Comonad.Cofree (unwrap)
import           Text.EditDistance

type RenamerM a = ExceptT RenamerError (ReaderT String (State RenamerState)) a

type RenamedModule a = ModuleBG N.QualifiedName a

data RenamerState = RS
  { boundNames      :: [(String, N.QualifiedName)]
  , boundTypes      :: [(String, N.QualifiedName)]
  , boundSignatures :: [(String, N.QualifiedName)]
  } deriving (Show, Eq)

data RenamerError
  = NotBound Name
  | AlreadyBound QualifiedName
  | AlreadyBoundSig QualifiedName
  | ErrorInValue Name RenamerError
  | ErrorInInstance Name RenamerError
  | ErrorInData Name RenamerError
  | ErrorInSignature Name RenamerError
  | ErrorInTraitDecl Name RenamerError
  deriving (Show)

instance Pretty RenamerError where
  pretty (NotBound         nm      ) = pretty "Unknown name" <+> dquotes (pretty nm)
  pretty (AlreadyBound     nm      ) = pretty "Duplicate definition for" <+> pretty nm
    -- SHould be ambiguous name rather than duplicate definition
    -- Hints: qualify import, change name, qualify name at use site
  pretty (AlreadyBoundSig  nm      ) = pretty "Duplicate signature for" <+> pretty nm
  pretty (ErrorInValue     nm error) = nest 2 $ vcat $
    [ pretty "Error in the value" <+> pretty nm <> colon
    , pretty error
    ]
  pretty (ErrorInSignature nm error) = nest 2 $ vcat $
    [ pretty "Error in the value signature" <+> ticks (pretty nm) <> colon
    , pretty error
    ]
  pretty (ErrorInInstance  nm error) = nest 2 $ vcat $
    [ pretty "Error in the trait instance" <+> ticks (pretty nm) <> colon
    , pretty error
    ]
  pretty (ErrorInData      nm error) = nest 2 $ vcat $
    [ pretty "Error in the type definition" <+> ticks (pretty nm) <> colon
    , pretty error
    ]
  pretty (ErrorInTraitDecl nm error) = nest 2 $ vcat $
    [ pretty "Error in the trait declaration" <+> ticks (pretty nm) <> colon
    , pretty error
    ]

renameModule ::  Module' (BoundModules Name a) -> Either (Error ann) (Module' (BoundModules N.QualifiedName a))
renameModule mod = fst <$> runRenamer mod

runRenamer :: Module' (BoundModules Name a) -> Either (Error ann) (Module' (BoundModules N.QualifiedName a), RenamerState)
runRenamer mod = renameModule' (RS (fromBuiltin Builtins.builtins) (fromBuiltin Builtins.builtinTypes) []) mod
  where
  fromBuiltin =  map (\b -> (qualName $ fst b, fst b))

renameModule' :: RenamerState -> Module' (BoundModules Name a) -> Either (Error ann) (Module' (BoundModules N.QualifiedName a), RenamerState)
renameModule' rs mod = first (Module (moduleName mod)) <$> (runRenamer $ renameBindingGroups (moduleDecls mod))
  where
  runRenamer exp = let (res, state) = flip runState rs . flip runReaderT (moduleName mod) $ runExceptT exp
    in second (\x -> (x, state)) (first (fromErr state) res)

  fromErr rs err = Error
    { errKind    = "renamer"
    , errHeader  = header err
    , errSummary = pretty err
    , errHints   = hints rs err
    }

  header (NotBound         nm    ) = pretty "Missing Name"
  header (AlreadyBound     nm    ) = pretty "Duplicate Definition"
  header (AlreadyBoundSig  nm    ) = pretty "Duplicate Signature"
  header (ErrorInValue     nm err) = header err
  header (ErrorInInstance  nm err) = header err
  header (ErrorInData      nm err) = header err
  header (ErrorInSignature nm err) = header err
  header (ErrorInTraitDecl nm err) = header err

  hints :: RenamerState -> RenamerError -> [Doc ann]
  hints rs (NotBound         nm    ) = pure $ pretty "Maybe you meant:" `above` (bulleted $
    map (\(_, (nm, qual)) ->
      ticks (pretty nm) <+> if isInternal qual then mempty else pretty "defined in" <+> pretty (qualModule qual)
    ) $ take 4 namesInCutoff)

    where
    isInternal (Internal _) = True
    isInternal _ = False
    namesInCutoff = takeWhile ((<= cutoff) . fst) $ sortOn fst $
      map (\n -> (editDistance (fst n), n)) (boundNames rs)
    editDistance = levenshteinDistance defaultEditCosts nm
    cutoff = floor ((fromIntegral $ length nm) * 0.5)

  hints rs (AlreadyBound     nm    ) = mempty
  hints rs (AlreadyBoundSig  nm    ) = mempty
  hints rs (ErrorInValue     nm err) = hints rs err
  hints rs (ErrorInInstance  nm err) = hints rs err
  hints rs (ErrorInData      nm err) = hints rs err
  hints rs (ErrorInSignature nm err) = hints rs err
  hints rs (ErrorInTraitDecl nm err) = hints rs err

  -- TODO: Check for any remaining unbound signatures!

renameBindingGroups :: BoundModules Name a -> RenamerM (BoundModules N.QualifiedName a)
renameBindingGroups (BoundModules
  { classDecls = classDecls
  , instDecls  = instDecls
  , valueDecls = valueDecls
  , otherDecls = otherDecls
  , dataDecls  = dataDecls
  }) = do

  dataDecls'  <- mapM renameBindingGroup dataDecls
  classDecls' <- mapM renameBindingGroup classDecls
  valueDecls' <- mapM renameBindingGroup valueDecls
  instDecls'  <- mapM renameBindingGroup instDecls
  otherDecls' <- mapM renameBindingGroup otherDecls

  return $ BoundModules classDecls' instDecls' valueDecls' otherDecls' dataDecls'

renameType :: Type Name -> RenamerM (Type QualifiedName)
renameType (TVar t) = pure . TVar $ Internal t
renameType (TAp a b) = TAp <$> renameType a <*> renameType b
renameType (TConstructor nm) = TConstructor <$> qualifyTypeName nm
renameType (Arrow l r) = Arrow <$> renameType l <*> renameType r
renameType (Constrained cons ty) = Constrained <$> mapM renameConstraint cons <*> renameType ty
renameType (TUnknown uk) = pure $ TUnknown uk
renameType (Forall vars ty) = Forall <$> (mapM unsafeQualifyName vars) <*> renameType ty
renameType (ArrowConstructor) = pure ArrowConstructor

renameConstraint :: Constraint Name -> RenamerM (Constraint QualifiedName)
renameConstraint (nm, ty) = (,) <$> qualifyName nm <*> renameType ty

renameBindingGroup :: BindingGroup Name a -> RenamerM (BindingGroup N.QualifiedName a)
renameBindingGroup (ValueBG decls) = mapM (bindName . declName . unwrap) (filter isValue decls) >>
  ValueBG <$> mapM renameDeclaration decls
renameBindingGroup (DataBG  decls) = mapM (bindType . declName . unwrap) decls >> DataBG <$> mapM renameDeclaration decls
renameBindingGroup (OtherBG decl) = OtherBG <$> renameDeclaration decl

renameDeclaration :: Decl Name a -> RenamerM (Decl N.QualifiedName a)
renameDeclaration (a :< Value nm ds) = rethrow (ErrorInValue nm) $ do
  nm' <- qualifyName nm
  ds' <- mapM renameEqn ds

  return (a :< Value nm' ds')
renameDeclaration (a :< d@Data{}) = rethrow (ErrorInData (declName d)) $ do
  nm' <- qualifyTypeName (declName d)

  let unwrappedConses = map unwrapProduct (dataConstructors d)
      consNames = map ((\(TConstructor nm) -> nm ) . head) unwrappedConses
  bindNames consNames
  renamedConses <- mapM (\(TConstructor c : args) -> (:) <$> (TConstructor <$> qualifyName c) <*> mapM renameType args) unwrappedConses
  let cons' = map (foldl1 TAp) renamedConses

  let vars' = map Internal (dataVars d)

  return $ a :< d { dataVars = vars', declName = nm' , dataConstructors = cons' }
renameDeclaration (a :< d@Signature{}) = rethrow (ErrorInSignature (declName d)) $ do
  bindSignature (declName d)
  nm' <- qualifyName (declName d)
  ty' <- renameType (declType d)
  return $ a :< Signature nm' ty'
renameDeclaration (a :< d@Import{}) = undefined -- eventually use this to get information about bound names
renameDeclaration (a :< d@TypeSynonym{}) = undefined
renameDeclaration (a :< d@TraitDecl{}) = rethrow (ErrorInTraitDecl (traitName d)) $ do
  supertraits <- mapM renameConstraint (traitSuperclasses d)
  bindName (traitName d)
  nm' <- qualifyName (traitName d)
  vals' <- mapM renameDeclaration (traitValues d)
  let var' = Internal (traitVar d)
  return $ a :< d { traitSuperclasses = supertraits, traitName = nm', traitVar = var', traitValues = vals' }
renameDeclaration (a :< d@TraitImpl{}) = rethrow (ErrorInInstance (traitName d)) $ do
  supertraits <- mapM renameConstraint (traitSuperclasses d)

  nm' <- qualifyName (traitName d)
  vals' <- mapM renameDeclaration (traitValues d)
  ty' <- renameType (traitType d)

  return $ a :< d { traitSuperclasses = supertraits, traitName = nm', traitType = ty', traitValues = vals' }

renameEqn :: (Patterns Name a, Expr' Name a) -> RenamerM (Patterns QualifiedName a, Expr' QualifiedName a)
renameEqn (pats, val) = withScope $ do
  pats' <- renamePatterns pats
  val' <- renameExpression val

  return (pats', val')

renamePatterns :: Patterns Name a -> RenamerM (Patterns QualifiedName a)
renamePatterns = mapM renamePattern

renamePattern :: Pat' Name a -> RenamerM (Pat' QualifiedName a)
renamePattern (a :< Destructor nm pats)  = liftM (a :<) $ Destructor <$> qualifyName nm <*> renamePatterns pats
renamePattern (a :< Wildcard) = pure $ a :< Wildcard
renamePattern (a :< PVar nm)  = bindShadowing nm >> liftM (a :<) (PVar <$> qualifyName nm)
renamePattern (a :< PLit lit) = pure $ a :< PLit lit

renameExpression :: Expr' Name a -> RenamerM (Expr' QualifiedName a)
renameExpression (a :< Var nm) = (:<) a . Var <$> qualifyName nm
renameExpression (a :< Constructor nm) =  (:<) a . Constructor <$> qualifyName nm
renameExpression (a :< Assign nms exps) = do
  mapM bindInternalName nms
  nms' <- qualifyNames nms
  exps' <- mapM renameExpression exps
  return $ a :< Assign nms' exps'
renameExpression (a :< Apply f args) =
  liftM (a :<) (Apply <$> renameExpression f <*> mapM renameExpression args)
renameExpression (a :< BinOp op left right) =
  liftM (a :<) (BinOp <$> renameExpression op <*> renameExpression left <*> renameExpression right)
renameExpression (a :< Case scrutinee eqns) = do
  scrut' <- renameExpression scrutinee

  eqns' <- mapM (\(pat, exp) -> withScope $ (,) <$> renamePattern pat <*> renameExpression exp) eqns
  return $ a :< Case scrut' eqns'
renameExpression (a :< If cond left right) = do
  cond' <- renameExpression cond
  left' <- withScope $ renameExpression left
  right' <- withScope $ renameExpression right

  return $ a :< If cond' left' right'
renameExpression (a :< Lambda binders exp) =
  liftM (a :<) (withScope $ Lambda <$> renamePatterns binders <*> renameExpression exp)
renameExpression (a :< Literal lit) = pure $ a :< Literal lit
renameExpression (a :< Body exps) = withScope . liftM (a :<) $ Body <$> mapM renameExpression exps

-- | Monadic helper functions

qualifyName :: Name -> RenamerM QualifiedName
qualifyName nm = checkName nm

unsafeQualifyName :: Name -> RenamerM QualifiedName
unsafeQualifyName nm = Qualified <$> ask <*> pure nm

checkName :: Name -> RenamerM QualifiedName
checkName nm = do
  bound <- gets (\rs -> lookup nm (boundNames rs) <|> lookup nm (boundSignatures rs))
  case bound of
    Nothing -> throwError $ NotBound nm
    Just nm -> return nm

qualifyTypeName :: Name -> RenamerM QualifiedName
qualifyTypeName nm = do
  bound <- gets (\rs -> lookup nm (boundTypes rs))
  case bound of
    Nothing -> throwError $ NotBound nm
    Just nm -> return nm

bindName :: Name -> RenamerM ()
bindName nm = do
  bound <- gets (lookup nm . boundNames)

  case bound of
    Just qual -> throwError $ AlreadyBound qual
    Nothing -> unsafeQualifyName nm >>= \qual -> modify $ \rs ->  rs { boundNames = (nm, qual) : boundNames rs }

bindInternalName :: Name -> RenamerM ()
bindInternalName nm = do
  bound <- gets (lookup nm . boundNames)

  case bound of
    Just qual -> throwError $ AlreadyBound qual
    Nothing -> modify $ \rs ->  rs { boundNames = (nm, Internal nm) : boundNames rs }

bindType :: Name -> RenamerM ()
bindType nm = do
  bound <- gets (lookup nm . boundTypes)

  case bound of
    Just qual -> throwError $ AlreadyBound qual
    Nothing -> unsafeQualifyName nm >>= \qual -> modify $ \rs ->  rs { boundTypes = (nm, qual) : boundTypes rs }

bindShadowing :: Name -> RenamerM ()
bindShadowing nm = do
  modify $ \rs ->  rs { boundNames = (nm, Internal nm) : boundNames rs }

bindSignature :: Name -> RenamerM ()
bindSignature nm = do
  bound <- gets (lookup nm . boundSignatures)
  case bound of
    Just qual -> throwError $ AlreadyBoundSig qual
    Nothing -> unsafeQualifyName nm >>= \qual -> modify $ \rs ->  rs { boundSignatures = (nm, qual) : boundSignatures rs }

bindNames nms = mapM bindName nms

withScope act = do
  state <- get
  val <- act
  put state
  return val

qualifyNames = mapM qualifyName


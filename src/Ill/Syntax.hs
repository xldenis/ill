{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PatternSynonyms, StandaloneDeriving       #-}

module Ill.Syntax
( module X
, module Ill.Syntax
, Cofree(..)
, pretty
, extract
, unwrap
) where

import           Ill.Prelude

import           Control.Comonad.Cofree
import           Control.Comonad
import           Control.Lens.TH

import           Ill.Syntax.Expression as X
import           Ill.Syntax.Literal as X
import           Ill.Syntax.Pattern as X
import           Ill.Syntax.Type as X
import           Ill.Syntax.Kind as X
import           Ill.Syntax.Name as X

import           Control.Lens           (each, over, _2)
import           Ill.Syntax.Pretty

import           Text.Megaparsec (SourcePos(..), unPos)

import           Data.Bifunctor
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Functor.Apply

type Prefix = String

type IsQualified = Bool

type Alias = Maybe String

data Module' decl = Module
  { moduleName :: Name
  , moduleImports :: [Import]
  , moduleDecls :: decl
  } deriving (Show, Functor)


data Import = Import
  { importQualified :: IsQualified
  , importMask :: Masks
  , importName :: Name
  , importAlias :: Alias
  } deriving (Show, Eq)

-- type ParsedModule = Module (Decl Name SourceSpan)
type Module nm a = Module' [Decl nm a]

deriving instance (Eq nm, Eq a) => Eq (Module nm a)

instance Apply Module' where
  (<.>) = moduleApply

moduleApply (Module nm i f) (Module _ _ decl) = Module nm i (f decl)

data Declaration nm a b
  = Data
    { declName :: nm
    , dataVars :: [nm]
    , dataConstructors :: [Type nm]
    }
  | TypeSynonym
    { declName :: nm
    , dataVars :: [nm]
    , aliasType :: (Type Name)
    }
  | Value
    { declName :: nm
    , declEqns :: [(Patterns nm a, Expr' nm a)]
    }
  | Signature
    { declName :: nm
    , declType :: (Type nm)
    }
  | TraitDecl
    { traitSuperclasses ::[Constraint nm]
    , traitName  :: nm
    , traitVar  :: nm
    , traitValues :: [b]
    }
  | TraitImpl
    { traitSuperclasses :: [Constraint nm]
    , traitName :: nm
    , traitType :: (Type nm)
    , traitValues :: [b]
    }
  deriving (Eq, Functor, Show, Traversable, Foldable, Generic1)

type Decl nm a = Cofree (Declaration nm a) a

data Masks
  = Hiding [Name]
  | Only  [Name]
  | All
  deriving (Eq, Show)

makePrisms ''Declaration


instance (Eq nm, Eq a) => Eq1 (Declaration nm a) where
  liftEq = liftEqDefault

instance (Show nm, Show a) => Show1 (Declaration nm a) where
  liftShowsPrec = liftShowsPrecDefault

instance Bifunctor (Declaration nm) where
  bimap :: forall a b c d. (a -> b) -> (c -> d) -> Declaration nm a c -> Declaration nm b d
  bimap l r (Data n nms tys) = Data n nms tys
  bimap l r (TypeSynonym n nms ty) = TypeSynonym n nms ty
  bimap l r (Value n brs) = Value n $ map helper brs
    where helper (pats, expr) = (fmap (fmap l) pats, nestedFmap l expr)
  bimap l r (Signature nm ty) = Signature nm ty
  bimap l r (TraitDecl cs n nms bs) = TraitDecl cs n nms (map r bs)
  bimap l r (TraitImpl cs n tys bs) = TraitImpl cs n tys (map r bs)

instance Bifoldable (Declaration nm) where
  bifoldMap l r (Value n brs) = foldMap helper brs
    where
    helper (pats, expr) = foldMap (foldMap l) pats `mappend` foldMap l expr
  bifoldMap l r val = bifoldMapDefault l r val

instance Bitraversable (Declaration nm) where
  bitraverse :: forall f a b c d. Applicative f => (a -> f c) -> (b -> f d) -> Declaration nm a b -> f (Declaration nm c d)
  bitraverse l _ (Value n brs) = Value n <$> traverse helper brs
    where helper (pats, exp) = (,) <$> (traverse (traverse l) pats) <*> (hoistAppToCofree l exp)
  bitraverse _ _ (Data n nms tys) = pure $ Data n nms tys
  bitraverse _ _ (TypeSynonym n nms ty) = pure $ TypeSynonym n nms ty
  bitraverse _ _ (Signature nm ty) = pure $ Signature nm ty
  bitraverse _ r (TraitDecl cs n nms bs) = TraitDecl cs n nms <$> traverse r bs
  bitraverse _ r (TraitImpl cs n tys bs) = TraitImpl cs n tys <$> traverse r bs

hoistAppToCofree :: (Bitraversable t, Applicative f, Functor f) => (a -> f b) -> Cofree (t a) a -> f (Cofree (t b) b)
hoistAppToCofree f = hoistAppToCofree2 f f

hoistAppToCofree2 :: (Bitraversable t, Applicative f, Functor f) => (a -> f b) -> (c -> f d) -> Cofree (t a) c -> f (Cofree (t b) d)
hoistAppToCofree2 f g (a :< e) = (:<) <$> g a <*> (bitraverse f (hoistAppToCofree2 f g) e)

data SourceSpan = SourceSpan {begin :: SourcePos, end :: SourcePos} deriving (Eq, Show)

instance Pretty SourceSpan where
  pretty (SourceSpan begin end) = pretty (sourceName begin) <> pretty ":" <> prettyPos begin <> pretty "-" <> prettyPos end
    where prettyPos pos = pretty (unPos $ sourceLine pos) <> pretty ":" <> pretty (unPos $ sourceColumn pos)

data TypedAnn = TyAnn { span :: Maybe SourceSpan, ty :: TypeAnn}
  deriving (Show, Eq)

instance Pretty TypedAnn where
  pretty (TyAnn span ty) = pretty "ann" <+> braces (pretty "span" <+> pretty span <+> pretty "ty" <+> (pretty $ ty))

pattern Ann x y = TyAnn (Just x) (Type y Nothing)
pattern SynAnn y = TyAnn Nothing (Type y Nothing)

data TypeAnn
  = Type { polyTy :: Type QualifiedName, instTy :: Maybe (Type QualifiedName) }
  | Kind Kind
  | None
  deriving (Show, Eq)

instance Pretty TypeAnn where
  pretty (Type polyTy instTy) = pretty polyTy <+> if isJust instTy
    then pretty "instantiated" <+> pretty instTy
    else mempty
  pretty (Kind k) = pretty k
  pretty (None)  = mempty

-- Spooky partial function. Use only when invariant holds
fromType :: TypeAnn -> Type QualifiedName
fromType (Type t _) = t
fromType _ = error "impossible expression has non-type annotation"

fromTyAnn :: TypedAnn -> Type QualifiedName
fromTyAnn = fromType . ty

typeOf :: Functor f => Cofree f TypedAnn -> (Type QualifiedName)
typeOf c =  fromMaybe (polyTy $ fromTy c) (instTy $ fromTy c)
  where fromTy = ty . extract

polyTyOf :: Functor f => Cofree f TypedAnn -> (Type QualifiedName)
polyTyOf = polyTy . ty . extract


instTyOf :: Functor f => Cofree f TypedAnn -> Maybe (Type QualifiedName)
instTyOf = instTy . ty . extract

getAnnSubst :: TypedAnn -> [(QualifiedName, Type QualifiedName)]
getAnnSubst t = subst
  where
  subst = case join $ subsume <$> (pure . unForall . polyTy $ ty t) <*> (instTy $ ty t) of
    Just l -> filter (\(v, _) -> v `elem` boundVars (polyTy $ ty t)) l
    Nothing -> []

  boundVars (Forall vs _) = vs
  boundVars _ = []

  unForall (Forall _ t) = t
  unForall t = t

valueName :: Decl nm a -> nm
valueName (_ :< Value n _) = n

isValue :: Decl nm a -> Bool
isValue (_ :< Value _ _) = True
isValue _ = False

isDataDecl :: Decl nm a -> Bool
isDataDecl (_ :< Data _ _ _) = True
isDataDecl _ = False

isSignature :: Decl nm a -> Bool
isSignature (_ :< Signature _ _) = True
isSignature _ = False

isImpl :: Decl nm a -> Bool
isImpl (_ :< TraitImpl _ _ _ _) = True
isImpl _ = False

isDecl :: Decl nm a -> Bool
isDecl (_ :< TraitDecl _ _ _ _) = True
isDecl _ = False

nestedFmap :: (Bifunctor f, Functor (f a)) => (a -> b) -> Cofree (f a) a -> (Cofree (f b) b)
nestedFmap f v = hoistCofree (first f) $ fmap f v

fmapTy f (Type t m) = Type (f t) m
fmapTy f t          = t

dropAnn :: (Bifunctor f, Functor (f a)) => Cofree (f a) a -> Cofree (f ()) ()
dropAnn = nestedFmap (const ())

lookupFn n (Module _ _ ds) = find pred ds
  where pred (_ :< Value name _) = n == name
        pred _                   = False

instance (Pretty (Type nm), Pretty nm) => Pretty (Module nm a) where
  pretty (Module name imports decls) = nest 2 (pretty "module" <+> pretty name `above`
    vsep (intersperse mempty (map pretty imports ++ map pretty decls))) `above`
    pretty "end"

instance Pretty Import where
  pretty (Import qual msk name alias) = pretty "import" <-> conditionally (const $ pretty "qualified") qual mempty
    <-> pretty name <-> prettyJust alias <-> prettyMask msk
    where prettyJust (Just alias') = pretty "as" <+> pretty alias'
          prettyJust  Nothing     = mempty
          prettyMask (Hiding nms) = pretty "hiding" <+> tupled (map pretty nms)
          prettyMask (Only   nms) = tupled $ map pretty nms
          prettyMask _            = mempty
instance (Pretty (Type nm), Pretty nm) => Pretty1 (Declaration nm a) where
  liftPretty pretty' (Data name vars cons) = pretty "data" <+> pretty name <+> hsep' (map pretty vars) <> pretty '=' <+> alternative (map pretty cons)
    where alternative = encloseSep mempty mempty (pretty " | ")
          hsep' [] = mempty
          hsep' xs = hsep xs <+> emptyDoc
  liftPretty pretty' (TypeSynonym alias vars target) = pretty "type" <+> pretty alias <+> pretty vars <+> pretty "=" <+> pretty target
  liftPretty pretty' (Value name cases) = vsep (headBranch : map otherBranch (tail cases)) `above` pretty "end"
    where branch (args, body) = nest 2 $ tupled (map pretty args) `above` pretty body
          headBranch    = pretty "fn" <+> pretty name <+> branch (head cases)
          otherBranch b = pretty "or" <+> pretty name <+> branch b
  liftPretty pretty' (Signature func tp) = pretty func <+> pretty "::" <+> pretty tp

  liftPretty pretty' (TraitDecl super name arg body)  = nest 2 (pretty "trait" <+> declarationLine `above` vsep (map pretty' body)) `above` pretty "end"
    where constraints c = if null c then mempty else hsep (punctuate comma (map pretty c)) <+> pretty "|"
          declarationLine = constraints super <+> pretty name <+> pretty arg
  liftPretty pretty' (TraitImpl supers trtNm arg body) = nest 2 (declarationLine `above` vsep (map pretty' body)) `above` pretty "end"
    where
    constraints c = if null c then mempty else hsep (punctuate comma (map prettyCons c)) <+> pretty "|"
    prettyCons (nm, ty) = pretty nm <+> pretty ty
    declarationLine = pretty "impl" <+> constraints supers <+> pretty trtNm <+> pretty arg

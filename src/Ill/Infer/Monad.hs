{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
module Ill.Infer.Monad
( module Ill.Infer.Monad
, module Control.Monad.Unify
, module Ill.Error
, runWriterT
) where

import           Ill.Prelude

import           Ill.Error
import           Ill.Parser.Lexer     (SourceSpan (..))
import           Ill.Syntax
import           Ill.Syntax.Pretty
import           Ill.Syntax.Name

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer (runWriterT)
import           Control.Monad.Unify

import           Ill.Syntax.Builtins
import           Data.Map (Map, insert, union, fromList, (!?), (!), insertWith, adjust)

import           Data.Data
import           Data.Bifunctor (first)

data Environment = Environment
  { names             :: Map QualifiedName (Type QualifiedName)
  , types             :: Map QualifiedName Kind
  , constructors      :: Map QualifiedName ConstructorEntry
  , traits            :: TraitDict
  , traitDictionaries :: InstanceDict
  } deriving (Show, Eq)

type TraitDict = [(QualifiedName, TraitEntry)]

data ConstructorEntry = ConstructorEntry
  { consName :: QualifiedName
  , consType :: Type QualifiedName
  , consTyVars :: [QualifiedName]
  , consArity :: Int
  , consTag :: Int
  } deriving (Show, Eq, Data)

data TraitEntry = TraitEntry
  { superTraits :: [Constraint QualifiedName]
  , traitVarNm  :: QualifiedName
  , methodSigs :: [(QualifiedName, Type QualifiedName)]
  } deriving (Show, Eq)

type InstanceDict = Map QualifiedName [InstanceEntry] -- [(Name, [InstanceEntry])]

data InstanceEntry = InstanceEntry
  { instType        :: Type QualifiedName
  , instConstraints :: [Constraint QualifiedName]
  , instName        :: QualifiedName
  } deriving (Show, Eq)

data CheckState = CheckState
  { env     :: Environment
  , nextVar :: Int
  } deriving (Show, Eq)

newtype Check a = Check { runCheck :: StateT CheckState (Except CheckError) a}
  deriving (Functor, Applicative, Monad, MonadError CheckError, MonadState CheckState)

execCheck state c = first fromCheckError . runExcept $ flip runStateT state (runCheck c)

defaultCheckEnv = CheckState (Environment (fromList builtins) mempty mempty mempty mempty) 0

-- | Errors

data CheckError
  = UnificationError (Type QualifiedName) (Type QualifiedName)
  | InternalError String
  | UndefinedType QualifiedName
  | UndefinedTrait QualifiedName
  | UndefinedVariable QualifiedName
  | UndefinedConstructor QualifiedName
  | NotImplementedError String
  | KindUnificationError Kind Kind
  | KindOccursError Kind
  | TypeOccursError (Type QualifiedName)
  | MissingTraitImpl [Constraint QualifiedName]
  | ErrorInExpression (Expr  SourceSpan) (CheckError)
  | ErrorInPattern (Pat SourceSpan) (CheckError)
  | ErrorInDecl QualifiedName CheckError
  | InsufficientConstraints [Constraint QualifiedName]
  | AmbiguousType [Name] (Type QualifiedName)
  | MissingSuperTraits [Constraint QualifiedName] QualifiedName (Type QualifiedName)
  | MissingTraitImplMethods QualifiedName (Type QualifiedName) [(QualifiedName, Type QualifiedName)]
  | UnknownTraitMethods QualifiedName (Type QualifiedName) [QualifiedName]
  deriving (Show)

notImplementedError :: (MonadError CheckError m) => String -> m a
notImplementedError = throwError . NotImplementedError

fromCheckError :: CheckError -> Error a
fromCheckError err = Error
  { errHeader  = pretty (header err)
  , errSummary = pretty err
  , errKind    = "typechecker"
  , errHints   = hints err
  }

  where
  hints (AmbiguousType{}) =
    [ wrappedWords "An ambiguous variable is a type-variable that appears in a constraint but doesn't in the type itself.\
      \ That means that we can't ever figure out a value for it since nothing 'ties' it to the type."
    ]
  hints (InsufficientConstraints{}) =
    [ wrappedWords "Try adding the missing constraints to the signature of the declaration\
      \ or remove calls to the methods that introduce the constraints."
    ]
  hints (UnknownTraitMethods _ _ nms) =
    [ wrappedWords "Define helper methods outside of trait implementations."
    ]
  hints (ErrorInExpression _ e)    = hints e
  hints (ErrorInPattern _ e)       = hints e
  hints (ErrorInDecl _ e)          = hints e

  hints _ = []

  wrappedWords = fillSep . map pretty . words

  header UnificationError{}         = "Unification Error"
  header InternalError{}            = "Internal Error"
  header UndefinedType{}            = "UndefinedType"
  header UndefinedTrait{}           = "Undefined Trait"
  header UndefinedVariable{}        = "Undefined Variable"
  header UndefinedConstructor{}     = "Undefined Constructor"
  header NotImplementedError{}      = "Not Yet Implemented"
  header KindUnificationError{}     = "Kind Unification Error"
  header KindOccursError{}          = "Kind Occurs Error"
  header TypeOccursError{}          = "Type Occurs Error"
  header MissingTraitImpl{}         = "Missing Trait Implementation"
  header InsufficientConstraints{}  = "Insufficient Constraints"
  header AmbiguousType{}            = "Ambiguous Type"
  header MissingSuperTraits{}       = "Missing Super Traits for Implementation"
  header MissingTraitImplMethods{}  = "Trait implementation is missing required methods"
  header UnknownTraitMethods{}      = "Trait implementation contains unknown methods"
  header (ErrorInExpression _ e)    = header e
  header (ErrorInPattern _ e)       = header e
  header (ErrorInDecl _ e)          = header e
  -- header e = error $ "Non-exhaustive function header, constructor:" ++ (show e)

instance Pretty CheckError where
  pretty (InternalError s) = pretty "internal error" <+> pretty s
  pretty (UnificationError t1 t2) = pretty "Unification error could not" <+> hang 1 doc
    where doc = vsep [pretty "unify:" <+> pretty t1, pretty "with:" <+> pretty t2]
  pretty (ErrorInExpression location error) = vcat $
    [ pretty "Error in the expression at" <+> pretty (extract location) <> pretty ":"
    , pretty location
    , (nest 2 $ pretty error)
    ]
  pretty (ErrorInPattern location error) = vcat $
    [ pretty "Error in the pattern at" <+> pretty (extract location) <> pretty ":"
    , pretty location
    , (nest 2 $ pretty error)
    ]
  pretty (ErrorInDecl name error) = vcat . map (dot' <+>) $
    [ pretty "Error in the top-level declaration" <+> (ticks (pretty name)) <> pretty ":"
    , (nest 2 $ pretty error)
    ]
  pretty (MissingTraitImpl [p]) = pretty "missing trait impl: " <+> pretty p
  pretty (InsufficientConstraints cons) = nest 2 $
    -- what we _do_ have in the context
    pretty "The context for the declaration is missing necessary constraints:" `above`
      bulleted (map pretty cons)
  pretty (AmbiguousType ambiguities ty) =
    pretty "The following type variables are ambiguous:"
      <+> list (map pretty ambiguities)
      <+> nest 2 (hardline <> dot' <+> pretty "Full type:" <+> pretty ty)
  pretty (MissingSuperTraits supers traitNm args) =
    pretty "The trait implementation for" <+> pretty traitNm <+> pretty args <+> pretty "has unsatisfied super-trait constraints:"
    `above` bulleted (map pretty supers)
  pretty (MissingTraitImplMethods traitNm args methods) =
    pretty "The trait implementation for" <+> pretty traitNm <+> pretty args <+> pretty "has missing methods:"
    `above` bulleted (map (\(nm, ty) -> pretty nm <+> pretty "::" <+> pretty ty) methods)
  pretty (UnknownTraitMethods traitNm args methodNames) =
    pretty "The trait implementation for" <+> pretty traitNm <+> pretty args <+> pretty "has unknown methods:"
    `above` bulleted (map pretty methodNames)

  pretty s = pretty $ show s

-- | Helper functions for inference

putEnv :: MonadState CheckState m => Environment -> m ()
putEnv e = modify (\s -> s { env  = e })

getEnv :: MonadState CheckState m => m Environment
getEnv = env <$> get

localState :: MonadState s m => (s -> s) -> m a -> m a
localState f action = do -- get rid of this it doesn't work
  orig <- get
  modify f
  a <- action
  put orig
  return a

liftUnify :: Partial t => UnifyT t Check a -> Check (a, Substitution t)
liftUnify action = do
  st <- get
  let ut = runUnify (defaultUnifyState { unifyNextVar = nextVar st}) action
  (a, ust) <- ut
  modify $ \st -> st { nextVar = unifyNextVar ust }
  let uust = unifyCurrentSubstitution ust
  return (a, uust)

lookupVariable :: (MonadError CheckError m, MonadState CheckState m) => QualifiedName -> m (Type QualifiedName)
lookupVariable name = do
  env <- getEnv
  case names env !? name of
    Nothing -> throwError $ UndefinedVariable name
    Just a  -> return a

lookupConstructor :: (MonadError CheckError m, MonadState CheckState m) => QualifiedName -> m ConstructorEntry
lookupConstructor name = do
  env <- getEnv
  case constructors env !? name of
    Nothing -> throwError $ UndefinedConstructor name
    Just a  -> return a

lookupTypeVariable ::  (MonadError CheckError m, MonadState CheckState m) => QualifiedName -> m Kind
lookupTypeVariable name = do
  env <- getEnv
  case types env !? name of
    Nothing -> throwError $ UndefinedType name
    Just a  -> return a

lookupTrait :: (MonadError CheckError m, MonadState CheckState m) => QualifiedName -> m TraitEntry
lookupTrait name = do
  env <- getEnv
  case lookup name (traits env) of
    Nothing -> throwError $ UndefinedTrait name
    Just a  -> return a

bindNames :: MonadState CheckState m => [(QualifiedName, Type QualifiedName)] -> m a -> m a
bindNames nms action = do
  orig <- get
  modify (\s -> s { env = (env s) { names = (fromList nms) `union` names (env s) } })
  a <- action
  modify (\s -> s { env = (env s) { names = names . env $ orig } })
  return a

bindTypeVariables :: MonadState CheckState m => [(QualifiedName, Kind)] -> m a -> m a
bindTypeVariables tyVars action = do
  orig <- get
  modify (\s -> s { env = (env s) { types = types (env s) `union` (fromList tyVars) } })
  a <- action
  modify (\s -> s { env = (env s) { types = types . env $ orig } })
  return a

addNames bound = modifyEnv $ \e -> e { names = fromList bound `union` names e }

modifyEnv f = modify $ \st -> st { env = f (env st) }

addTrait :: MonadState CheckState m => QualifiedName -- class name
  -> [Constraint QualifiedName] -- super classes
  -> QualifiedName -- variables of class
  -> [(QualifiedName, Type QualifiedName)]  -- name and type of member sigs
  -> m ()
addTrait name supers arg members = do
  let qualifiedMembers = map (fmap (generalize . qualifyType . generalizeWithout [arg])) members

  modifyEnv $ \e -> e { traits = (name, TraitEntry supers arg qualifiedMembers) : traits e }
  modifyEnv $ \e -> e { names = (fromList qualifiedMembers) `union` names e }
  where
  qualifyType t = Constrained fullConstraints t
  fullConstraints = (name, TVar arg) : supers

withTraitInstance :: MonadState CheckState m => QualifiedName -> [Constraint QualifiedName] -> Type QualifiedName -> m a -> m a
withTraitInstance trait supers inst action = do
  environment <- env <$> get

  putEnv $ environment { traitDictionaries = insertWith (++) trait [InstanceEntry inst supers trait] (traitDictionaries environment) }
  a <- action

  modifyEnv (\env' -> env' { traitDictionaries = traitDictionaries environment })

  return a

addTraitInstance :: QualifiedName -> [Constraint QualifiedName] -> Type QualifiedName -> Check ()
addTraitInstance trait supers inst = do
  env <- env <$> get

  putEnv $ env { traitDictionaries = insertWith (++) trait [InstanceEntry inst supers trait] (traitDictionaries env) }

addValue :: QualifiedName -> Type QualifiedName -> Check ()
addValue name ty = do
  env <- env <$> get
  let env' = env { names = insert name ty (names env)  }
  modify $ \s -> s { env = env' }

addDataType :: QualifiedName -> [QualifiedName] -> [(QualifiedName, [Type QualifiedName])] -> Kind -> Check ()
addDataType name args dctors ctorKind = do
  env <- env <$> get
  let value = ctorKind
  let env' = env { types = insert name ctorKind (types env) }
  modify $ \s -> s { env = env' }

  zipWithM_ (\i -> uncurry $ addDataConstructor name args i ) [0..] dctors

addDataConstructor :: QualifiedName -> [QualifiedName] -> Int -> QualifiedName -> [Type QualifiedName] -> Check ()
addDataConstructor tyCons args tag dataCons tys = do
  env <- env <$> get
  let retTy = foldl TAp (TConstructor tyCons) (map TVar args)
      dataConsTy = generalize $ foldr tFn retTy tys
      fields = args
      consEntry = ConstructorEntry tyCons dataConsTy fields (length tys) tag
  putEnv $ env { constructors = insert dataCons consEntry (constructors env) }
  return ()

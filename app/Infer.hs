{-# LANGUAGE OverloadedStrings #-}
module Infer where

import Ill.Options
import Ill.Syntax
import Ill.Infer
import Ill.Infer.Monad
import Ill.BindingGroup
import Ill.Error
import Ill.Syntax.Pretty

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)

import Data.List (nub)
import Data.Text.Lazy.IO
import Data.Text.Lazy hiding (map)
import qualified Data.Map as M

import Prelude hiding (putStrLn, putStr)

infer :: GlobalOptions -> Module SourceSpan -> IO ()
infer gOpts m = case runTC m of
    Left e ->
      case e of
        otherwise -> putStrLn . renderError gOpts $ prettyError e
    Right (ts, checkState) -> do
      let typeToInsts = instanceMap (traitDictionaries $ env checkState)
      putStrLn "\nTraits\n"
      void $ forM (traits . env $ checkState) $ putStrLn . prettyTraitInfo gOpts

      printBG gOpts typeToInsts ts

runTC (Module _ ds) = bindingGroups ds >>= execCheck . typeCheck

unCheck c = execCheck c

printBG :: GlobalOptions -> M.Map String [InstanceEntry] -> [BindingGroup TypedAnn] -> IO ()
printBG opts m ((ValueBG ds):bgs) = printTypes opts m ds >> printBG opts m bgs
printBG opts m ((DataBG  ds):bgs) = printTypes opts m ds >> printBG opts m bgs
printBG opts m (_ : bgs) = printBG opts m bgs
printBG opts _ []        = return ()

printTypes :: GlobalOptions -> M.Map String [InstanceEntry] -> [Decl TypedAnn] -> IO ()
printTypes opts m ((a :< Value n _):ts)   = putStr (pack n <> ": ") >> putStrLn (renderError opts . pretty $ ty a) >> printTypes opts m ts
printTypes opts m ((a :< Data  n _ _):ts) = putStr (pack n <> ": ") >> print (nest 2 $ (pretty $ ty a) `above` tyInsts) >> printTypes opts m ts
  where tyInsts = prettyTraitInsts $ M.findWithDefault [] n m
printTypes opts m (_ : ts) = printTypes opts m ts
printTypes _ _ [] = return ()

renderError opts = renderIll (renderArgs opts)

prettyTraitInfo opts (nm, TraitEntry supers args mems) =
  let topRow = pretty nm <+> (hsep $ map pretty args)
      mems'  = map (\(memNm, ty) -> pretty memNm <+> "::" <+> pretty ty) mems
  in renderIll (renderArgs opts) (nest 2 $ topRow `above` vsep mems')

prettyTraitInsts (insts) = vsep insts'
  where insts' = map (pretty . instHead) (nub insts)
        instHead :: InstanceEntry -> Type Name
        instHead inst = constrain (instConstraints inst) (TAp (TConstructor (instName inst)) (instType inst))

instanceMap id = M.foldrWithKey accTyInstances mempty id
  where accTyInstances traitNm insts acc = Prelude.foldr (\x -> insertInstanceForType traitNm x) acc insts

insertInstanceForType :: String -> InstanceEntry -> M.Map String [InstanceEntry] -> M.Map String [InstanceEntry]
insertInstanceForType traitNm inst acc = let
  -- this is partial
  TConstructor tyName = Prelude.head $ unwrapProduct (instType inst)
  in M.insertWith (++) tyName [inst] acc

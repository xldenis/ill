{-# LANGUAGE OverloadedStrings #-}
module Infer where

import Ill.Options
import Ill.Syntax
import Ill.Infer
import Ill.Infer.Monad
import Ill.BindingGroup
import Ill.Error
import Ill.Renamer
import Ill.Syntax.Pretty

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)

import Data.List (nub)
import Data.Text.Lazy.IO
import Data.Text.Lazy hiding (map)
import qualified Data.Map as M

import Prelude hiding (putStrLn, putStr)

infer :: GlobalOptions -> RenamedModule SourceSpan -> IO ()
infer gOpts m = case runTC m of
    Left e ->
      case e of
        otherwise -> putStrLn . renderError gOpts $ prettyError e
    Right (ts, checkState) -> do
      let typeToInsts = instanceMap (traitDictionaries $ env checkState)
      putStrLn "\nTraits\n"
      void $ forM (traits . env $ checkState) $ putStrLn . prettyTraitInfo gOpts

      putStrLn "\nInstances\n"

      print $ vcat $ map (\(traitNm, insts) -> nest 2 $ (pretty $ traitNm) `above` (prettyTraitInsts insts)) (M.toList (traitDictionaries $ env checkState))

      putStrLn "\nTypes\n"
      printBG gOpts typeToInsts ts

runTC m = (execCheck . typeCheck . moduleDecls) m

printBG :: GlobalOptions -> M.Map QualifiedName [InstanceEntry] -> [BindingGroup QualifiedName TypedAnn] -> IO ()
printBG opts m ((ValueBG ds):bgs) = printTypes opts m ds >> printBG opts m bgs
printBG opts m ((DataBG  ds):bgs) = printTypes opts m ds >> printBG opts m bgs
printBG opts m (_ : bgs) = printBG opts m bgs
printBG opts _ []        = return ()

printTypes :: GlobalOptions -> M.Map QualifiedName [InstanceEntry] -> [Decl QualifiedName TypedAnn] -> IO ()
printTypes opts m ((a :< Value n _):ts)   = putStr (pack . show $ pretty n <> text ": ") >> putStrLn (renderError opts . pretty $ ty a) >> printTypes opts m ts
printTypes opts m ((a :< Data  n _ _):ts) = putStr (pack . show $ pretty n <> text ": ") >> print (pretty $ ty a) >> printTypes opts m ts
  where tyInsts = prettyTraitInsts $ M.findWithDefault [] n m
printTypes opts m (_ : ts) = printTypes opts m ts
printTypes _ _ [] = return ()

renderError opts = renderIll (renderArgs opts)

prettyTraitInfo opts (nm, TraitEntry supers args mems) =
  let topRow = pretty nm <+> (pretty args)
      mems'  = map (\(memNm, ty) -> pretty memNm <+> "::" <+> pretty ty) mems
  in renderIll (renderArgs opts) (nest 2 $ topRow `above` vsep mems')

prettyTraitInsts (insts) = vsep insts'
  where insts' = map (pretty . instHead) (nub insts)
        instHead :: InstanceEntry -> Type QualifiedName
        instHead inst = constrain (instConstraints inst) (TAp (TConstructor (instName inst)) (instType inst))

instanceMap :: InstanceDict -> M.Map QualifiedName [InstanceEntry]
instanceMap id = M.foldrWithKey accTyInstances mempty id
  where accTyInstances traitNm insts acc = Prelude.foldr (\x -> insertInstanceForType traitNm x) acc insts

insertInstanceForType :: QualifiedName -> InstanceEntry -> M.Map QualifiedName [InstanceEntry] -> M.Map QualifiedName [InstanceEntry]
insertInstanceForType traitNm inst acc = let
  -- this is partial
  TConstructor tyName = Prelude.head $ unwrapProduct (instType inst)
  in M.insertWith (++) tyName [inst] acc

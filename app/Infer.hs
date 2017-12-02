{-# LANGUAGE OverloadedStrings #-}
module Infer where

import Ill.Syntax
import Ill.Infer
import Ill.Infer.Monad
import Ill.BindingGroup
import Ill.Error
import Ill.Syntax.Pretty

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)
import Data.List (nub)

import Prelude hiding (putStrLn, putStr)
import Data.Text.Lazy.IO
import Data.Text.Lazy hiding (map)
import qualified Data.Map as M

infer m = let
  typed = runTC m
  in case typed of
    Left e ->
      case e of
        otherwise -> putStrLn $ prettyType e
    Right (ts, checkState) -> do
      let typeToInsts = instanceMap (traitDictionaries $ env checkState)
      putStrLn "\nTraits\n"
      void $ forM (traits . env $ checkState) $ putStrLn . prettyTraitInfo

      printBG typeToInsts ts

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck)

unCheck c = execCheck c

printBG m ((ValueBG ds):bgs) = printTypes m ds >> printBG m bgs
printBG m ((DataBG  ds):bgs) = printTypes m ds >> printBG m bgs
printBG m (_ : bgs) = printBG m bgs
printBG _ []        = return ()

printTypes :: M.Map String [InstanceEntry] -> [Decl TypedAnn] -> IO ()
printTypes m ((a :< Value n _):ts)   = putStr (pack n <> ": ") >> putStrLn (prettyType $ ty a) >> printTypes m ts
printTypes m ((a :< Data  n _ _):ts) = putStr (pack n <> ": ") >> print (nest 2 $ (pretty $ ty a) `above` tyInsts) >> printTypes m ts
  where tyInsts = prettyTraitInsts $ M.findWithDefault [] n m
printTypes m (_ : ts) = printTypes m ts
printTypes _ [] = return ()

prettyType a = renderIll defaultRenderArgs (pretty $ a)

prettyTraitInfo (nm, TraitEntry supers args mems) =
  let topRow = pretty nm <+> (hsep $ map pretty args)
      mems'  = map (\(memNm, ty) -> pretty memNm <+> "::" <+> pretty ty) mems
  in renderIll defaultRenderArgs (nest 2 $ topRow `above` vsep mems')

prettyTraitInsts (insts) = vsep insts'
  where insts' = map (pretty . instHead) (nub insts)
        instHead :: InstanceEntry -> Type Name
        instHead inst = constrain (instConstraints inst) (Prelude.foldl TAp (TConstructor (instName inst)) (instTypes inst))

instanceMap id = M.foldrWithKey accTyInstances mempty id
  where accTyInstances traitNm insts acc = Prelude.foldr (\x -> insertInstanceForType traitNm x) acc insts

insertInstanceForType :: String -> InstanceEntry -> M.Map String [InstanceEntry] -> M.Map String [InstanceEntry]
insertInstanceForType traitNm inst acc = let
  -- this is partial
  TConstructor tyName = Prelude.head $ unwrapProduct (Prelude.head $ instTypes inst)
  in M.insertWith (++) tyName [inst] acc

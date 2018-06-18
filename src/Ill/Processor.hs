{-# LANGUAGE ScopedTypeVariables #-}
module Ill.Processor where
import Data.Graph
import Ill.Syntax
import Ill.Prelude

import Ill.BindingGroup
import Ill.Renamer
import Ill.Infer
import Ill.Desugar
import Data.Tuple
import Data.Traversable

import Debug.Trace

sortModulesByImports :: Show a => [Module' a] -> [Module' a]
sortModulesByImports mods = if not isDag
  then error "import cycle!"
  else
    let (g, f, _) = graphFromEdges graphList
    in map (fstOf3 . f) (topSort $ transposeG g)
  where
  fstOf3 (a, _, _) = a
  isDag = all (isAcyclic) (stronglyConnComp graphList)
  isAcyclic (AcyclicSCC _) = True
  isAcyclic _ = False
  graphList = map graphNode mods
  graphNode mod = (mod, moduleName mod, map importName (moduleImports mod))

modulesToCore mods = do
  groupedModules <- mapM bindingGroups mods

  (_, renamedModules) <- mapAccumLM (\state mod -> do
    swap <$> runRenamer state (moduleName mod) (renameModule mod)
    ) defaultRenamerState groupedModules

  (_, coreModules) <- mapAccumLM (\state mod -> do
    (mod', env) <- runTypecheckModule state mod
    let coreModule = compileCore (defaultPipeline env mod')
    return (CheckState env 0, coreModule)
    ) defaultCheckEnv renamedModules

  return coreModules

-- | Monadic version of mapAccumL
mapAccumLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining function
            -> acc                      -- ^ initial state
            -> [x]                      -- ^ inputs
            -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM _ s []     = return (s, [])
mapAccumLM f s (x:xs) = do
    (s1, x')  <- f s x
    (s2, xs') <- mapAccumLM f s1 xs
    return    (s2, x' : xs')

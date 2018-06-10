module Ill.Syntax.Builtins where

import Ill.Syntax.Type
import Ill.Prelude

builtins = primitives ++
  [ ("==", generalize $ constrain [("Eq", TVar "a")]   $ TVar "a" `tFn` TVar "a" `tFn` tBool)
  , ("<=", generalize $ constrain [("Ord", TVar "a")] $ TVar "a" `tFn` TVar "a" `tFn` tBool)
  , (">=", generalize $ constrain [("Ord", TVar "a")] $ TVar "a" `tFn` TVar "a" `tFn` tBool)
  , ("<",  generalize $ constrain [("Ord", TVar "a")] $ TVar "a" `tFn` TVar "a" `tFn` tBool)
  , (">",  generalize $ constrain [("Ord", TVar "a")] $ TVar "a" `tFn` TVar "a" `tFn` tBool)
  , ("+",  generalize $ constrain [("Semigroup", TVar "a")] $ TVar "a" `tFn` TVar "a" `tFn` TVar "a")
  , ("-",  generalize $ constrain [("Group", TVar "a")] $ TVar "a" `tFn` TVar "a" `tFn` TVar "a")
  , ("*",  generalize $ constrain [("MultSemigroup", TVar "a")] $ TVar "a" `tFn` TVar "a" `tFn` TVar "a")
  ]

primitives =
  [ ("plusInt",       tInteger `tFn` tInteger `tFn` tInteger)
  , ("minusInt",      tInteger `tFn` tInteger `tFn` tInteger)
  , ("multInt",       tInteger `tFn` tInteger `tFn` tInteger)
  , ("divInt",        tInteger `tFn` tInteger `tFn` tInteger)
  , ("eqInt",         tInteger `tFn` tInteger `tFn` tBool)
  , ("ltInt",         tInteger `tFn` tInteger `tFn` tBool)
  , ("gtInt",         tInteger `tFn` tInteger `tFn` tBool)
  , ("leqInt",        tInteger `tFn` tInteger `tFn` tBool)
  , ("geqInt",        tInteger `tFn` tInteger `tFn` tBool)
  , ("maxInt",        tInteger `tFn` tInteger `tFn` tInteger)
  , ("minInt",        tInteger `tFn` tInteger `tFn` tInteger)
  , ("plusDouble",    tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("minusDouble",   tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("multDouble",    tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("divDouble",     tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("eqDouble",      tDouble  `tFn` tDouble  `tFn` tBool)
  , ("ltDouble",      tDouble  `tFn` tDouble  `tFn` tBool)
  , ("gtDouble",      tDouble  `tFn` tDouble  `tFn` tBool)
  , ("leqDouble",     tDouble  `tFn` tDouble  `tFn` tBool)
  , ("geqDouble",     tDouble  `tFn` tDouble  `tFn` tBool)
  , ("maxDouble",     tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("minDouble",     tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("plusStr",       tString  `tFn` tString  `tFn` tString)
  , ("showInt",       tInteger `tFn` tString)
  , ("failedPattern", generalize $ TVar "a")
  , ("omgDebug",      tString `tFn` tString)
  ]

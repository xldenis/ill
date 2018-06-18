{-# LANGUAGE OverloadedStrings #-}
module Ill.Syntax.Builtins where

import Ill.Syntax.Type
import Ill.Syntax.Kind
import Ill.Syntax.Name
import Ill.Prelude
import Data.String (IsString)

builtinTypes :: [(QualifiedName, Kind)]
builtinTypes =
  [ (Qualified "Prelude" "Int", Star)
  , (Qualified "Prelude" "String", Star)
  , (Qualified "Prelude" "Char", Star)
  ]

builtins :: [(QualifiedName, Type QualifiedName)]
builtins = primitives ++ map (\(nm, ty) -> (Qualified "Prelude" nm, ty))
  [ ("==", generalize $ constrain [(prelude "Eq",  tVarA)] $ tVarA `tFn` tVarA `tFn` tBool)
  , ("<=", generalize $ constrain [(prelude "Ord", tVarA)] $ tVarA `tFn` tVarA `tFn` tBool)
  , (">=", generalize $ constrain [(prelude "Ord", tVarA)] $ tVarA `tFn` tVarA `tFn` tBool)
  , ("<",  generalize $ constrain [(prelude "Ord", tVarA)] $ tVarA `tFn` tVarA `tFn` tBool)
  , (">",  generalize $ constrain [(prelude "Ord", tVarA)] $ tVarA `tFn` tVarA `tFn` tBool)
  , ("+",  generalize $ constrain [(prelude "Semigroup",     tVarA)] $ tVarA `tFn` tVarA `tFn` tVarA)
  , ("-",  generalize $ constrain [(prelude "Group",         tVarA)] $ tVarA `tFn` tVarA `tFn` tVarA)
  , ("*",  generalize $ constrain [(prelude "MultSemigroup", tVarA)] $ tVarA `tFn` tVarA `tFn` tVarA)
  , ("/",  generalize $ constrain [(prelude "MultGroup",     tVarA)] $ tVarA `tFn` tVarA `tFn` tVarA)
  , ("&&", generalize $ tBool `tFn` tBool `tFn` tBool)
  , ("||", generalize $ tBool `tFn` tBool `tFn` tBool)
  , ("failedPattern", generalize $ tVarA)
  ]
  where
  prelude nm = Qualified "Prelude" nm
  tVarA = TVar $ Internal "a"

primitives :: [(QualifiedName, Type QualifiedName)]
primitives = map (\(nm, ty) -> (Qualified "Prelude" nm, ty))
  [ ("plusInt",       tInteger `tFn` tInteger `tFn` tInteger)
  , ("minusInt",      tInteger `tFn` tInteger `tFn` tInteger)
  , ("multInt",       tInteger `tFn` tInteger `tFn` tInteger)
  , ("divInt",        tInteger `tFn` tInteger `tFn` tInteger)
  , ("modInt",        tInteger `tFn` tInteger `tFn` tInteger)
  , ("eqInt",         tInteger `tFn` tInteger `tFn` tBool)
  , ("ltInt",         tInteger `tFn` tInteger `tFn` tBool)
  , ("gtInt",         tInteger `tFn` tInteger `tFn` tBool)
  , ("leqInt",        tInteger `tFn` tInteger `tFn` tBool)
  , ("geqInt",        tInteger `tFn` tInteger `tFn` tBool)

  , ("plusDouble",    tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("minusDouble",   tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("multDouble",    tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("divDouble",     tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("modDouble",     tDouble  `tFn` tDouble  `tFn` tDouble)
  , ("eqDouble",      tDouble  `tFn` tDouble  `tFn` tBool)
  , ("ltDouble",      tDouble  `tFn` tDouble  `tFn` tBool)
  , ("gtDouble",      tDouble  `tFn` tDouble  `tFn` tBool)
  , ("leqDouble",     tDouble  `tFn` tDouble  `tFn` tBool)
  , ("geqDouble",     tDouble  `tFn` tDouble  `tFn` tBool)

  , ("plusStr",       tString  `tFn` tString  `tFn` tString)
  , ("eqStr",         tString  `tFn` tString  `tFn` tBool)
  , ("lenStr",        tString  `tFn` tInteger)
  , ("showInt",       tInteger `tFn` tString)
  , ("omgDebug",      tString `tFn` tString)
  ]

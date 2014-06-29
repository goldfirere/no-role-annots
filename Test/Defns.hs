{- Testing definitiosn in no-role-annots package.
   Copyright 2014 Richard Eisenberg

   https://github.com/goldfirere/no-role-annots/
-}

{-# LANGUAGE TemplateHaskell, GADTs #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-warnings-deprecations #-}

module Test.Defns (MyMap1, MyMap2, mkMap2, MyPtr1, MyPtr2, MyMap3, MyMap4) where

import Language.Haskell.RoleAnnots

data MyMap1 k v = MkMyMap1 [(k,v)]
data (Nominal k, Representational v) => MyMap2 k v = MkMyMap2 [(k,v)]
data MyPtr1 a = MkMyPtr1 Int
data Representational a => MyPtr2 a = MkMyPtr2 Int

roleAnnot [NominalR, RepresentationalR]
  [d| newtype MyMap3 k v = MkMyMap3 [(k,v)] |]

newtype (Nominal k, Representational v) => MyMap4 k v = MkMyMap4 [(k,v)]

-- check that compilation works:
mkMap2 :: k -> v -> MyMap2 k v
mkMap2 k v = MkMyMap2 [(k,v)]

matchMap2 :: MyMap2 k v -> ()
matchMap2 (MkMyMap2 _) = ()

{- Testing definitions in no-role-annots package.
   Copyright 2014 Richard Eisenberg

   https://github.com/goldfirere/no-role-annots/
-}

{-# LANGUAGE TemplateHaskell, GADTs, CPP #-}

module Test.Test where

#if __GLASGOW_HASKELL__ >= 707
import Language.Haskell.TH
#endif

import Language.Haskell.RoleAnnots
import Language.Haskell.RoleAnnots.Check

import System.Exit

data MyMap1 k v = MkMyMap1 [(k,v)]
data MyMap2 k v = (Nominal k, Representational v) => MkMyMap2 [(k,v)]
data MyPtr1 a = MkMyPtr1 Int
data MyPtr2 a = Representational a => MkMyPtr2 Int

checkRoles ''MyMap1 [RepresentationalR, RepresentationalR]
checkRoles ''MyMap2 [NominalR, RepresentationalR]
checkRoles ''MyPtr1 [PhantomR]
checkRoles ''MyPtr2 [RepresentationalR]

-- checkRoles ''MyMap1 [NominalR, RepresentationalR]  -- should report error

main :: IO ()
main = do
  putStrLn "It compiled!"
  let actual = [ $(checkRolesB ''MyMap1 [NominalR, RepresentationalR])
               , $(checkRolesB ''MyMap2 [NominalR, RepresentationalR])
               , $(checkRolesB ''MyPtr1 [RepresentationalR]) ]
#if __GLASGOW_HASKELL__ < 707
      desired = [True, True, True]
#else
      desired = [False, True, False]
#endif
  putStrLn $ "Actual  checkRolesB: " ++ show actual
  putStrLn $ "Desired checkRolesB: " ++ show desired

  let failed_appropriately =
#if __GLASGOW_HASKELL__ < 707
        True
#else
        $( do recover [| True |] $
                do _ <- checkRoles ''MyMap1 [NominalR, RepresentationalR]
                   [| False |] )
#endif

  putStrLn $ "Did checkRoles fail appropriately? " ++ show failed_appropriately
  
  if actual == desired && failed_appropriately
  then exitSuccess
  else exitFailure

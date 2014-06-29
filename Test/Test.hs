{- Testing definitions in no-role-annots package.
   Copyright 2014 Richard Eisenberg

   https://github.com/goldfirere/no-role-annots/
-}

{-# LANGUAGE TemplateHaskell, CPP, GeneralizedNewtypeDeriving,
             StandaloneDeriving #-}

module Test.Test where

#if __GLASGOW_HASKELL__ < 707
import Language.Haskell.RoleAnnots
#else
import Language.Haskell.TH
#endif

import Test.Defns
import Language.Haskell.RoleAnnots.Check

import System.Exit

class C a where
  mymap :: MyMap2 String a

instance C Int where
  mymap = mkMap2 "Foo" 3

newtype Age = MkAge Int
  deriving C

class D a where
  mymap' :: MyMap2 a String

instance D Int where
  mymap' = mkMap2 3 "Foo"

-- deriving instance D Age   -- shouldn't work
  
checkRoles ''MyMap1 [RepresentationalR, RepresentationalR]
checkRoles ''MyMap2 [NominalR, RepresentationalR]
checkRoles ''MyPtr1 [PhantomR]
checkRoles ''MyPtr2 [RepresentationalR]
checkRoles ''MyMap3 [NominalR, RepresentationalR]
checkRoles ''MyMap4 [NominalR, RepresentationalR]

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

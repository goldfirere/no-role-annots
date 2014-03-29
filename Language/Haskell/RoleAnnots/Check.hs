-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.RoleAnnots.Check
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module uses Template Haskell to check whether a declared type
-- has the desired roles. (In versions of GHC before roles, these checks
-- always succeed.)
--
----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Language.Haskell.RoleAnnots.Check (
  checkRoles, checkRolesB,

  -- | 'Role' is re-exported from Template Haskell for convenience.
  Role(..)
  ) where

import Language.Haskell.TH.Syntax

#if __GLASGOW_HASKELL__ < 707
import Language.Haskell.RoleAnnots   ( Role(..) )
#else
import Language.Haskell.TH.Ppr
import Control.Monad  ( when )
#endif

-- | This function ensures that a declared type has a desired set of roles.
-- Call it in a top-level Template Haskell splice, like this:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > module MyMap where
-- >
-- > import Language.Haskell.RoleAnnots
-- > import Language.Haskell.RoleAnnots.Check
-- >
-- > data MyMap k v = (Nominal k, Representational v) => ...
-- >
-- > $(checkRoles ''MyMap [NominalR, RepresentationalR])
--
-- If the roles are not as desired, the 'checkRoles' will cause a compile-
-- time error.
--
-- The two quote marks there are Template Haskell syntax to
-- quote an in-scope name. Also, due to the way Template Haskell works,
-- the declaration you are checking must come before the call to 'checkRoles'.
--
-- 'checkRoles' may be called in a separate module from where the datatype
-- of interest is defined. It might be useful, for example, in a testsuite.
checkRoles :: Name -> [Role] -> Q [Dec]
checkRoles _n _desired = do
#if __GLASGOW_HASKELL__ >= 707
  actual <- reifyRoles _n 
  when (actual /= _desired) $
    reportError $ "Inferred roles of " ++ show _n ++ " differ from desired roles.\n"
               ++ "      Inferred: " ++ _print_list (map pprint actual) ++ "\n"
               ++ "      Desired:  " ++ _print_list (map pprint _desired)
#endif
  return []

  where
    _print_list []     = "[]"
    _print_list [x]    = "[" ++ x ++ "]"
    _print_list (x:xs) = "[" ++ x ++ go xs
      where
        go []     = "]"
        go (y:ys) = ", " ++ y ++ go ys

-- | This function is like 'checkRoles', but it can be used in a context
-- expecting a @Bool@ value, like this:
--
-- > rolesAreCorrect :: Bool
-- > rolesAreCorrect = $(checkRolesB ''MyMap [NominalR, RepresentationalR])
--
-- 'checkRolesB' never produces a compile-time error.
checkRolesB :: Name -> [Role] -> Q Exp
checkRolesB _n _desired = do
#if __GLASGOW_HASKELL__ < 707
  return (ConE trueName)
#else
  actual <- reifyRoles _n
  if actual == _desired
  then return (ConE trueName)
  else return (ConE falseName)
#endif

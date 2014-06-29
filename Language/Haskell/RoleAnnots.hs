-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.RoleAnnots
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides three class constraints that can be used to make
-- roles stricter in your datatypes. Here is a typical use case:
--
-- > -- Use an association list as a map:
-- > data (Nominal k, Representational v) => Map k v = MkMap [(k,v)]
--
-- With a declaration such as this, GHC will assign correct roles to @k@
-- and @v@. In versions of GHC before roles, these constraints have no
-- effect. You will need to make sure the language extension
-- @-XDatatypeContexts@ is enabled. This extension is enabled by default
-- with a default language of either Haskell98 or Haskell2010, but not
-- by default with a vanilla invocation of GHC. When enabling it manually,
-- you may also want to specify @-fno-warn-deprecated-flags@, as datatype
-- contexts are generally a misfeature.
--
-- Note that these constraints can only make roles stricter, such as a
-- change from representational to nominal. Indeed, going the other way
-- would not be type-safe! Thus, there is no guarantee that a parameter
-- has the role given -- the guarantee is only that a parameter as
-- /at least/ the role given. (Thus, 'Phantom' is always redundant and
-- is included only for completeness.)
--
-- To check that the roles are what you would
-- expect, see module "Language.Haskell.RoleAnnots.Check".
--
----------------------------------------------------------------------------

{-# LANGUAGE CPP, PolyKinds, FlexibleInstances, Trustworthy #-}
#if __GLASGOW_HASKELL__ < 707
{-# LANGUAGE DeriveDataTypeable #-}
#else
{-# LANGUAGE IncoherentInstances, RoleAnnotations #-}
#endif

module Language.Haskell.RoleAnnots (
  Nominal, Representational, Phantom,
  roleAnnot, Role(..)
  ) where

import Language.Haskell.TH

#if __GLASGOW_HASKELL__ < 707
import Data.Data  ( Data, Typeable )
#endif

-- | Mark a type parameter as having a nominal role
class Nominal a

-- | Mark a type parameter as having a representational role
class Representational a

-- | Mark a type parameter as having a phantom role. (This is always redundant.)
class Phantom a

instance Nominal a
instance Representational a
instance Phantom a

#if __GLASGOW_HASKELL__ >= 707
type role Nominal nominal        -- this annotation is redundant
type role Representational representational
type role Phantom phantom
#endif

{-# DEPRECATED roleAnnot "The `roleAnnot` function is not necessary. Use a role constraint in a datatype context instead." #-}
-- | Deprecated since role inference looks at datatype contexts (with the release
-- of GHC 7.8).
roleAnnot :: [Role] -> Q [Dec] -> Q [Dec]
roleAnnot _roles qdecs = do
#if __GLASGOW_HASKELL__ < 707
  qdecs
#else
  decs <- qdecs
  case decs of
    [DataD _ name _ _ _]    -> return $ RoleAnnotD name _roles : decs
    [NewtypeD _ name _ _ _] -> return $ RoleAnnotD name _roles : decs
    _                       -> (reportError $ "Please pass exactly one \"data\" "
                                          ++ "or \"newtype\" declaration to "
                                          ++ "roleAnnot.") >>
                               return decs
#endif

#if __GLASGOW_HASKELL__ < 707
-- | This declaration mirrors the declaration within Template Haskell, for
-- use in earlier versions of GHC.
data Role = NominalR | RepresentationalR | PhantomR | InferR
  deriving (Show, Eq, Data, Typeable)
#endif

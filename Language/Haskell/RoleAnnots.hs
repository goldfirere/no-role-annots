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
-- > data Map k v =
-- >   (Nominal k, Representational v) => MkMap [(k,v)]
--
-- With a declaration such as this, GHC will assign correct roles to @k@
-- and @v@. In versions of GHC before roles, these constraints have no
-- effect. The constraints need be put on only one data constructor, though
-- there is no harm in duplicating them.
--
-- Note that these constraints can only make roles stricter, such as a
-- change from representational to nominal. Indeed, going the other way
-- would not be type-safe! Thus, there is no guarantee that a parameter
-- has the role given -- the guarantee is only that a parameter as
-- /at least/ the role given. (Thus, 'Phantom' is always redundant and
-- is included only for completeness.)
--
-- Unfortunately, the trick above does not work for @newtype@s, which
-- cannot accept constraints. The only solution there is Template Haskell.
-- Use the function 'roleAnnot'.
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

-- | A backward-compatible (almost) role annotation. To use, write code like
-- this:
--
-- > roleAnnot [NominalR, RepresentationalR]
-- >   [d| newtype MyMap k v = MkMyMap [(k,v)] |]
--
-- You will, of course, need @-XTemplateHaskell@.
--
-- The "almost" above refers to the fact that this will require the
-- @-XRoleAnnotations@ extension. How to avoid using @CPP@ in this case?
-- Put the extension in your .cabal file, like this:
--
-- > if impl(ghc >= 7.8)
-- >   default-extensions: RoleAnnotations
--
-- (Cabal files need no @endif@ -- they use indentation to detect the
-- body of the conditional.)
--
-- You can check this out in action in the
-- <https://github.com/goldfirere/no-role-annots/blob/master/no-role-annots.cabal cabal file> for no-role-annots (in the @test-suite@ section).
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
data Role = NominalR | RepresentationalR | PhantomR
  deriving (Show, Eq, Data, Typeable)
#endif
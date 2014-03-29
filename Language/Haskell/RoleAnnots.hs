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
-- To check that the roles are what you would
-- expect, see module "Language.Haskell.RoleAnnots.Check".
--
----------------------------------------------------------------------------

{-# LANGUAGE CPP, PolyKinds, FlexibleInstances #-}
#if __GLASGOW_HASKELL__ < 707
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy, IncoherentInstances, RoleAnnotations #-}
#endif

module Language.Haskell.RoleAnnots (
  Nominal, Representational, Phantom
  ) where

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

`no-role-annots`
================

This package is intended to ease the transition from GHC 7.6.3- to GHC 7.8+
by providing a backward-compatible role annotation syntax. The module
`Language.Haskell.RoleAnnots` exports backward-compatible declarations
usable to assign stricter roles to declarations. Proper role annotations
are preferred, but these work, too.

The module `Language.Haskell.RoleAnnots.Check` provides a way to check
that a datatype has a set of desired roles. This might be suitable for
use in a testsuite.

See the module documentation for details.
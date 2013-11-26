module Jvmm.Scope where
import Jvmm.Scope.Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- EXPORTED HELPERS --
----------------------
-- Tags given identifier as if it occured in global scope.
tagGlobal :: UIdent -> UIdent
tagGlobal = tagWith tag0

-- Prepares an identifier that could not be provided by the user from given one.
tempIdent :: UIdent -> String -> UIdent
tempIdent id ctx = id +/+ "#" +/+ ctx

-- SCOPE COMPUTATION --
-----------------------
-- Creates scoped tree from translated AST.
scope :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
scope classes = fmap fst $ fmap fst $ runScopeM (collectClasses classes >> funH classes)


module Jvmm.Scope where
import Jvmm.Scope.Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- SCOPE COMPUTATION --
-----------------------
-- Creates scoped tree from translated AST.
scope :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
scope classes = fmap fst $ fmap fst $ runScopeM (enterHierarchy classes $ funH classes)


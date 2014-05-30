module Jvmm.Scope where
import Jvmm.Scope.Internal as Internal

import Control.Applicative
import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output (ClassHierarchy)

-- SCOPE RESOLUTION -----------------------------------------------------------
--  The layer responsible for scope resolution and interpretation of ambiguous
--  references (e.g. is it field access or a local variable?).
-------------------------------------------------------------------------------

scope :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
scope classes = (fst . fst) <$> runScopeM (enterHierarchy classes $ Internal.scope classes)


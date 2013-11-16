module Jvmm.Analyser where
import Jvmm.Analyser.Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Hierarchy.Output

-- TODO we should at least evaluate constant expressions and make
-- use of the fact that infinite while does not return

-- MAIN --
----------
analyse :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
analyse = return


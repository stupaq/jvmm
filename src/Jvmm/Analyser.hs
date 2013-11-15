module Jvmm.Analyser where
import Jvmm.Analyser.Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Hierarchy.Output

-- MAIN --
----------
analyse :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
-- FIXME
analyse = return


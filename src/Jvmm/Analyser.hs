module Jvmm.Analyser where
import Jvmm.Analyser.Internal as Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output

-- MAIN --
----------
analyse :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
analyse = runAnalyserM . Internal.analyse


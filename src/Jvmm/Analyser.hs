module Jvmm.Analyser where
import Jvmm.Analyser.Internal as Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output (ClassHierarchy)

-- ANALYSER -------------------------------------------------------------------
--  The layer responsible for performing static evaluation and optimization,
--  like prunning unreachable branches, propagating constants and partially
--  evaluating expressions, removing unreachable code and so on.
-------------------------------------------------------------------------------
analyse :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
analyse = runAnalyserM . Internal.analyse


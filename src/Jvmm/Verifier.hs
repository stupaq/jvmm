module Jvmm.Verifier where
import Jvmm.Verifier.Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Hierarchy.Output

-- MAIN --
----------
verify :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
verify classes = (runVerifierM $ funH classes) >> return classes

module Jvmm.Verifier where
import Jvmm.Verifier.Internal as Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output

-- MAIN --
----------
verify :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
verify classes = runVerifierM (Internal.verify classes) >> return classes


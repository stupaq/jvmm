module Jvmm.Verifier where
import Jvmm.Verifier.Internal as Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output

-- VERIFIER -------------------------------------------------------------------
--  Performs static verification of APT (e.g. checks that non-void function
--  always returns, there exists and entrypoint and so on).
-------------------------------------------------------------------------------

verify :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
verify classes = runVerifierM (Internal.verify classes) >> return classes


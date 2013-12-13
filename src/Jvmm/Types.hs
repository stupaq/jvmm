module Jvmm.Types where
import Jvmm.Types.Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output

-- TYPE CHECKER ---------------------------------------------------------------
--  The layer that checks types and annotates APT with type information.
-------------------------------------------------------------------------------
typing :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
typing classes = do
  env <- collectTypes classes
  runTypeM env $ tcheck classes


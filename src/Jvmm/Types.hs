module Jvmm.Types where
import Jvmm.Types.Internal

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- MAIN --
----------
typing :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
typing classes = do
  env <- collectTypes classes
  runTypeM env $ funH classes


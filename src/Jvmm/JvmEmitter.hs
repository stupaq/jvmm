module Jvmm.JvmEmitter where
import Jvmm.JvmEmitter.Internal
import Jvmm.JvmEmitter.Output

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- MAIN --
----------
emitJvm :: ClassHierarchy -> ErrorInfoT Identity [JasminAsm]
emitJvm = emitClasses


module Jvmm.JvmEmitter where
import Jvmm.JvmEmitter.Internal
import Jvmm.JvmEmitter.Output

import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

import Text.Show.Pretty (ppShow)

-- MAIN --
----------
emitJvm :: String -> ClassHierarchy -> ErrorInfoT Identity [JasminAsm]
emitJvm = emitTopLevelStatics


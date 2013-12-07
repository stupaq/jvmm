module Jvmm.JvmEmitter where
import Jvmm.JvmEmitter.Internal
import Jvmm.JvmEmitter.Output

import System.FilePath (takeBaseName)

import Control.Monad.Identity

import Jvmm.Workflows
import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- MAIN --
----------
emitJvm :: Configuration -> ClassHierarchy -> ErrorInfoT Identity [JasminAsm]
emitJvm (Configuration { configurationDebug = debug, configurationSource = source }) =
  let env = emitterenv0 { emitterenvOverrideClass = Just (ClassName $ takeBaseName source)
                        , emitterenvDebugStack = debug }
  in emitTopLevelStatics env


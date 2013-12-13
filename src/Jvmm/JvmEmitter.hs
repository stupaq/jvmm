module Jvmm.JvmEmitter where
import Jvmm.JvmEmitter.Internal
import Jvmm.JvmEmitter.Output

import System.FilePath (takeBaseName)

import Control.Monad.Identity

import Jvmm.Workflows
import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output

-- JVM EMITTER ----------------------------------------------------------------
--  The layer responsible for emitting Jasmin assembler for restricted subset
--  of JVMM that can be compiled into JVm bytecode.
-------------------------------------------------------------------------------

emitJvm :: Configuration -> ClassHierarchy -> ErrorInfoT Identity [JasminAsm]
emitJvm (Configuration { configurationDebug = debug, configurationSource = source }) =
  let env = emitterenv0 { emitterenvOverrideClass = Just (ClassName $ takeBaseName source)
                        , emitterenvDebugStack = debug }
  in emitTopLevelStatics env


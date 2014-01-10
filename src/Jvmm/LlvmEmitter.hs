module Jvmm.LlvmEmitter where
import Jvmm.LlvmEmitter.Internal
import Jvmm.LlvmEmitter.Output

import Control.Monad.Identity

import System.FilePath (takeBaseName)

import Jvmm.Errors (ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Workflows

-- LLVM EMITTER ----------------------------------------------------------------
--  The layer responsible for emitting LLVM IR for restricted subset of JVMM
--  that can be compiled into binary.
-------------------------------------------------------------------------------

emitLlvm :: Configuration -> ClassHierarchy -> ErrorInfoT Identity [LlvmModule]
emitLlvm (Configuration { configurationSource = source }) =
  let env = emitterenv0 { emitterenvModuleName = takeBaseName source }
  in emitHierarchy env


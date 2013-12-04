module Jvmm.JvmEmitter.Output where

import Jvmm.Trans.Output (ClassName(..))

-- JASMIN ASSEMBLER INPUT --
----------------------------
data JasminAsm = JasminAsm ClassName AsmCode
type AsmCode = [String]


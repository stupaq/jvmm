module Jvmm.JvmEmitter.Output where

import Jvmm.Trans.Output (ClassName(..))

-- JASMIN ASSEMBLER INPUT --
----------------------------
data JasminAsm = JasminAsm String [JasminLine]
  deriving (Show)

data JasminLine =
    JasminDirective String
  | JasminInstruction String
  | JasminComment String
  deriving (Show, Eq)


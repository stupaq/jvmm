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
  | JasminEmpty
  deriving (Show, Eq)

toJasmin (JasminDirective str) = "." ++ str ++ "\n"
toJasmin (JasminInstruction str) = "\t" ++ str ++ "\n"
toJasmin (JasminComment str) = "; " ++ str ++ "\n"
toJasmin JasminEmpty = "\n"


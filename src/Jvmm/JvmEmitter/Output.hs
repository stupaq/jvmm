module Jvmm.JvmEmitter.Output where

-- JASMIN ASSEMBLER INPUT --
----------------------------
data JasminAsm = JasminAsm String [JasminLine]
  deriving (Show)

data JasminLine =
    JasminDirective String
  | JasminInstruction String
  | JasminComment String
  | JasminLabel String
  | JasminEmpty
  deriving (Show, Eq)

toJasmin :: JasminLine -> String
toJasmin (JasminDirective str) = '\n':'.':str
toJasmin (JasminInstruction str) = '\n':'\t':str
toJasmin (JasminComment ('\n':str)) = '\n':';':' ':str
toJasmin (JasminComment str) = '\t':';':' ':str
toJasmin (JasminLabel str) = '\n':str ++ ":"
toJasmin JasminEmpty = "\n"


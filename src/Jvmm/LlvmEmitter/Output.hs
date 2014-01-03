module Jvmm.LlvmEmitter.Output where

-- LLVM ASSEMBLER INPUT --
--------------------------
data LlvmModule = LlvmModule String [LlvmLine]
  deriving (Show)

data LlvmLine =
    LlvmInstruction String
  | LlvmComment String
  | LlvmLabel String
  | LlvmEmpty
  deriving (Show, Eq)

isInstruction :: LlvmLine -> Bool
isInstruction (LlvmInstruction _) = True
isInstruction _ = False

toLlvm :: LlvmLine -> String
toLlvm (LlvmInstruction str) = '\n':'\t':str
toLlvm (LlvmComment ('\n':str)) = '\n':';':' ':str
toLlvm (LlvmComment str) = '\t':';':' ':str
toLlvm (LlvmLabel str) = '\n':str ++ ":"
toLlvm LlvmEmpty = "\n"


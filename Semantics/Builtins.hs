module Semantics.Builtins where
import Syntax.AbsJvmm (Ident(..))

-- Names of builtin functions
names = map Ident $ ["print", "printLine", "readLine"] ++ ["printInt", "readInt", "printString", "readString", "error"]


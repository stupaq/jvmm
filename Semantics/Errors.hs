module Semantics.Errors where
import Control.Monad.Error
import qualified Data.List as List
import Syntax.AbsJvmm

duplicateArg (Arg _ (Ident id)) = "duplicate argument name: " ++ id
unboundSymbol (Ident id) = "unbound symbol: " ++ id
redeclaredSymbol (Ident id) = "redeclared symbol in the same scope: " ++ id
globalNonDec = "non-declaration in global scope"
globalVarDec ids = "variable declaration in global scope: " ++ (concat $ List.intersperse "," $ map (\(Ident id) -> id) ids)

module Semantics.Errors where
import Control.Monad.Error
import qualified Data.List as List

duplicateArg id = "duplicate argument: " ++ (show id)
unboundSymbol id = "unbound symbol: " ++ (show id)
globalNonDec = "non-declaration in global scope"
globalVarDec ids = "variable declaration in global scope: " ++ (concat $ List.intersperse "," $ map show ids)

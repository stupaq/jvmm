module Semantics.Errors where
import Control.Monad.Error
import qualified Data.List as List

duplicateArg id = throwError $ "duplicate argument: " ++ (show id)
unboundVar id = throwError $ "unbound variable: " ++ (show id)
--globalNonDec = throwError $ "non-declaration in global scope"
globalVarDec ids = throwError $ "variable declaration in global scope: " ++ (concat $ List.intersperse "," $ map show ids)

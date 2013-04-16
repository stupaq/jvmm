module Semantics.Errors where
import Control.Monad.Error
import qualified Data.List as List
import Syntax.AbsJvmm

action `rethrow` except = action `catchError` (\_ -> throwError except)

duplicateArg id = "duplicate argument: " ++ (show id)
unboundSymbol id = "unbound symbol: " ++ (show id)
redeclaredSymbol id = "redeclared symbol in the same scope: " ++ (show id)
globalForbidden = "statement not allowed in global scope"
unknownType id = "unknown type: " ++ (show id)

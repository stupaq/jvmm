module Semantics.Errors where
import Control.Monad.Error
import qualified Data.List as List
import Syntax.AbsJvmm

action `rethrow` except = action `catchError` (\_ -> throwError except)

duplicateArg (Arg _ (Ident id)) = "duplicate argument name: " ++ id
unboundSymbol (Ident id) = "unbound symbol: " ++ id
redeclaredSymbol (Ident id) = "redeclared symbol in the same scope: " ++ id
globalForbidden = "statement not allowed in global scope"

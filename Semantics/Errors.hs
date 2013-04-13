module Semantics.Errors where
import Control.Monad.Error

type ErrorMsg = String

duplicateArg id = throwError $ "duplicate argument: " ++ (show id)
unboundVar id = throwError $ "unbound variable: " ++ (show id)

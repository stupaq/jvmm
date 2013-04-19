module Semantics.Errors where
import Control.Monad.Error
import qualified Data.List as List
import Syntax.AbsJvmm

-- Discards error and throws provided one
action `rethrow` except = action `catchError` (\_ -> throwError except)

-- Scope resolution errors
duplicateArg typ id = concat ["duplicate argument: ", show typ, " ", show id]
unboundSymbol id = "unbound symbol: " ++ (show id)
redeclaredSymbol id = "redeclared symbol in the same scope: " ++ (show id)
globalForbidden = "statement not allowed in global scope"

-- Type resolution errors
unknownMemberType typ id = concat ["unknown type: ", show typ, " . ", show id]
unknownSymbolType id = "unknown type: " ++ (show id)
subscriptNonArray = "not an array subscripted"
indexType = "invalid array index type"
unexpectedType expected actual = concat ["not matching type, expected: ", show expected, " actual ", show actual]
argumentsNotMatch expected actual = concat ["arguments do not match, expected: ", concat $ map show expected, " actual ", concat $ map show actual]
badArithType = "bad operands type for arithmetic operator"
danglingReturn = "dangling return"

-- Static exception checking
uncaughtException typ = "uncaught, undeclared exception " ++ (show typ)

-- Runtime errors (not user-visible exceptions)
noDefaultValue typ = "type " ++ (show typ) ++ " has no default value"
nullPointerException = "attempt to dereference null"
indexOutOfBounds ind = "index out of bounds: " ++ (show ind)

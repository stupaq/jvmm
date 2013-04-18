module Semantics.Errors where
import Control.Monad.Error
import qualified Data.List as List
import Syntax.AbsJvmm

action `rethrow` except = action `catchError` (\_ -> throwError except)

duplicateArg id = "duplicate argument: " ++ (show id)
unboundSymbol id = "unbound symbol: " ++ (show id)
redeclaredSymbol id = "redeclared symbol in the same scope: " ++ (show id)
globalForbidden = "statement not allowed in global scope"
unknownMemberType typ id = concat ["unknown type: ", show typ, " . ", show id]
unknownSymbolType id = "unknown type: " ++ (show id)
subscriptNonArray = "not an array subscripted"
indexType = "invalid array index type"
unexpectedType expected actual = concat ["not matching type, expected: ", show expected, " actual ", show actual]
argumentsNotMatch expected actual = concat ["arguments do not match, expected: ", concat $ map show expected, " actual ", concat $ map show actual]
badArithType = "bad operands type for arithmetic operator"
danglingReturn = "dangling return"
uncaughtException typ = "uncaucht, undeclared exception" ++ (show typ)

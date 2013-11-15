module Jvmm.Errors where

import Control.Monad.Identity
import Control.Monad.Error

-- We might want to carry more structure in errors
-- TODO switch all code to these classes
type ErrorInfo = String
type ErrorInfoT = ErrorT ErrorInfo
type ErrorInfoM = Either ErrorInfo
runErrorInfoM = runIdentity . runErrorT

-- Discards error and throws provided one
action `rethrow` except = action `catchError` (\_ -> throwError except)

-- Class hierarchy computation errors
redeclaredInSuper ids = "fields redeclared in super classes: " ++ (unwords $ map show ids)
redeclaredWithDifferentType ids = "method redeclared in super classes with not mathing type: " ++ (unwords $ map show ids)

-- Scope resolution errors
duplicateArg typ id = concat ["duplicate argument: ", show typ, " ", show id]
unboundSymbol id = "unbound symbol: " ++ (show id)
redeclaredSymbol id = "redeclared symbol in the same scope: " ++ (show id)
globalForbidden = "statement not allowed in global scope"

-- Type resolution errors
unknownMemberType typ id = concat ["unknown member type: ", show typ, " . ", show id]
unknownSymbolType id = "unknown symbol type: " ++ (show id)
subscriptNonArray = "not an array subscripted"
indexType = "invalid array index type"
unexpectedType expected actual = concat ["not matching type, expected: ", show expected, " actual ", show actual]
argumentsNotMatch expected actual = concat ["arguments do not match, expected: ", show expected, " actual: ", show actual]
badArithType = "bad operands type for arithmetic operator"
danglingReturn = "dangling return"
missingReturn = "missing return"
voidVar id = "a variable cannot have void type: " ++ (show id)
voidField id = "a field cannot have void type: " ++ (show id)
voidArg = "an argument cannot have void type"
voidNotIgnored = "void value not ignored as it should be"
incompatibleMain = "incompatible main() type"
redeclaredType id = "redeclared type name: " ++ (show id)
referencedPrimitive typ = "primitive type cannot be referenced: " ++ (show typ)
danglingThis = "dangling this"
noSuperType typ = "cannot determine super type for: " ++ (show typ)

-- Static exception checking
uncaughtException typ = "uncaught, undeclared exception " ++ (show typ)

-- Runtime errors (not user-visible exceptions)
noDefaultValue typ = "type " ++ (show typ) ++ " has no default value"
nullPointerException = "attempt to dereference null"
indexOutOfBounds ind = "index out of bounds: " ++ (show ind)
nonVoidNoReturn = "no return value from non-void function"
zeroDivision = "divided by zero"
intOverflow = "integer overflow"
isConstant id = "constant cannot be modified: " ++ (show id)

-- Terminal error issued by a user
userError = "error called"
uncaughtTopLevel obj = "uncaught top level exception: " ++ (show obj)

-- Impossible situations
danglingReference loc = "location: " ++ (show loc) ++ " does not exist"
fromJustFailure ctx = "fromJust failed in context: " ++ (show ctx)
unusedBranch ctx = "unused pattern branch entered: " ++ (show ctx)

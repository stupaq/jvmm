{-# LANGUAGE FlexibleContexts #-}
module Jvmm.Errors where

import Control.Monad.Identity
import Control.Monad.Error

-- ERROR LOCATION --
--------------------
data Location =
    Line Int
  | Range Int Int
  | Unknown
  deriving (Ord, Eq)

instance Show Location where
  show Unknown = "unknown location"
  show (Line no) = concat ["near line ", show no]
  show (Range start end) = concat ["between lines ", show start, " and ", show end]

-- CUSTOM ERROR TYPE --
-----------------------
data ErrorInfo =
    Located Location String
  | Dangling String

instance Show ErrorInfo where
  show (Located loc msg) = concat [show loc, ", error: ", msg]
  show (Dangling msg) = concat [show Unknown, ", error: ",  msg]

instance Error ErrorInfo where
  noMsg = Dangling "unknown error"
  strMsg = Dangling

-- ERROR MONAD --
-----------------
type ErrorInfoT = ErrorT ErrorInfo
type ErrorInfoM = Either ErrorInfo
runErrorInfoM = runIdentity . runErrorT

-- Discards error and throws provided one
action `rethrow` err = action `catchError` (\_ -> throwError err)

-- Discards error and returns provided value
action `orReturn` value = action `catchError` (\_ -> return value)

-- Executes second action in all circumstances (and only once)
action `finally` always = do
  res <- action `catchError` (\err -> always >> throwError err)
  always
  return res

-- Tags error with location information
withLocation :: (MonadError ErrorInfo m) => Location -> m a -> m a
withLocation loc action = action `catchError` handler
  where
    handler (Dangling msg) = throwError $ Located loc msg
    handler err@(Located _ _) = throwError err

-- ERROR MESSAGES --
--------------------
-- Class hierarchy computation errors
redeclaredInSuper ids = Dangling $ "fields redeclared in super classes: " ++ (unwords $ map show ids)
redeclaredWithDifferentType ids = Dangling $ "method redeclared in super classes with not mathing type: " ++ (unwords $ map show ids)

-- Scope resolution errors
staticNonStaticConflict id = Dangling $ "static and non-static symbol conflict: " ++ (show id)
duplicateArg id = Dangling $ concat ["duplicated argument identifier in function definition: ", show id]
unboundSymbol id = Dangling $ "unbound symbol: " ++ (show id)
redeclaredSymbol id = Dangling $ "redeclared symbol in the same scope: " ++ (show id)
globalForbidden = Dangling $ "statement not allowed in global scope"
repeatedDeclaration id = Dangling $ "repeated declaration for: " ++ (show id)
redefinedBuiltin id = Dangling $ "redefined built-in method: " ++ (show id)

-- Type resolution errors
unknownMemberType typ id = Dangling $ concat ["unknown member type: ", show typ, " . ", show id]
unknownSymbolType id = Dangling $ "unknown symbol type: " ++ (show id)
subscriptNonArray = Dangling $ "not an array subscripted"
indexType = Dangling $ "invalid array index type"
unexpectedType expected actual = Dangling $ concat ["not matching type, expected: ", show expected, " actual ", show actual]
argumentsNotMatch expected actual = Dangling $ concat ["arguments do not match, expected: ", show expected, " actual: ", show actual]
badArithType = Dangling $ "bad operands type for arithmetic operator"
danglingReturn = Dangling $ "dangling return"
missingReturn = Dangling $ "missing return"
voidVar id = Dangling $ "a variable cannot have void type: " ++ (show id)
voidField id = Dangling $ "a field cannot have void type: " ++ (show id)
voidArg = Dangling $ "an argument cannot have void type"
voidNotIgnored = Dangling $ "void value not ignored as it should be"
incompatibleMain = Dangling $ "incompatible main() type"
missingMain = Dangling $ "missing main() function"
redeclaredType id = Dangling $ "redeclared type name: " ++ (show id)
referencedPrimitive typ = Dangling $ "primitive type cannot be referenced: " ++ (show typ)
danglingThis = Dangling $ "dangling this"
noSuperType typ = Dangling $ "cannot determine super type for: " ++ (show typ)
intValueOutOfBounds n = Dangling $ "int constant out of bounds: " ++ (show n)

-- Static exception checking
uncaughtException typ = Dangling $ "uncaught, undeclared exception " ++ (show typ)

-- Runtime errors (not user-visible exceptions)
noDefaultValue typ = Dangling $ "type " ++ (show typ) ++ " has no default value"
nullPointerException = Dangling $ "attempt to dereference null"
indexOutOfBounds ind = Dangling $ "index out of bounds: " ++ (show ind)
nonVoidNoReturn = Dangling $ "no return value from non-void function"
zeroDivision = Dangling $ "divided by zero"
intOverflow = Dangling $ "integer overflow"
isConstant id = Dangling $ "constant cannot be modified: " ++ (show id)

-- Terminal error issued by a user
userError = Dangling $ "error called"
uncaughtTopLevel obj = Dangling $ "uncaught top level exception: " ++ (show obj)

-- Impossible situations
danglingReference loc = Dangling $ "location: " ++ (show loc) ++ " does not exist"
fromJustFailure ctx = Dangling $ "fromJust failed in context: " ++ (show ctx)

-- LOGIC ERRORS --
------------------
unusedBranch ctx = "unused pattern branch entered: " ++ (show ctx)


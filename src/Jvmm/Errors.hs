{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
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
  show (Line no) = "near line " ++ show no
  show (Range start end) = concat ["between lines ", show start, " and ", show end]

-- CUSTOM ERROR TYPE --
-----------------------
data ErrorInfo =
    Located Location String
  | Dangling String
  | Bare String
  deriving (Eq)

instance Show ErrorInfo where
  show (Located loc msg) = concat [show loc, ", error: ", msg]
  show (Dangling msg) = concat [show Unknown, ", error: ",  msg]
  show (Bare msg) = msg

instance Error ErrorInfo where
  noMsg = Dangling "unknown error"
  strMsg = Dangling

-- ERROR MONAD --
-----------------
type ErrorInfoT = ErrorT ErrorInfo
type ErrorInfoM = Either ErrorInfo
runErrorInfoM = runIdentity . runErrorT

-- Discards error and throws provided one
rethrow :: (MonadError e m) => m a -> e -> m a
action `rethrow` err = action `catchError` (\_ -> throwError err)

-- Discards error and throws provided one
orThrow :: (MonadError e m) => e -> m a -> m a
orThrow = flip rethrow

-- Discards error and returns provided value
orReturn :: (MonadError e m) => m a -> a -> m a
action `orReturn` value = action `catchError` (\_ -> return value)

-- Executes second action in all circumstances (and only once)
finally :: (MonadError e m) => m a -> m () -> m a
action `finally` always = do
  res <- action `catchError` (\err -> always >> throwError err)
  always
  return res

-- Returns true is given action executed without error
succeeded action = (action >> return True) `catchError` const (return False)

-- Tags error with location information
withLocation :: (MonadError ErrorInfo m) => Location -> m a -> m a
withLocation loc action = action `catchError` handler
  where
    handler (Dangling msg) = throwError $ Located loc msg
    handler err = throwError err

-- ERROR MESSAGES --
--------------------
-- Class hierarchy computation errors
redeclaredInSuper ids = Dangling $ "fields redeclared in super classes: " ++ unwords (map show ids)
redeclaredWithDifferentType ids = Dangling $ "method redeclared in super classes with not mathing type: " ++ unwords (map show ids)

-- Scope resolution errors
staticNonStaticConflict x = Dangling $ "static and non-static symbol conflict: " ++ show x
duplicateArg x = Dangling $ "duplicated argument identifier in function definition: " ++ show x
unboundSymbol x = Dangling $ "unbound symbol: " ++ show x
redeclaredSymbol x = Dangling $ "redeclared symbol in the same scope: " ++ show x
globalForbidden = Dangling "statement not allowed in global scope"
repeatedDeclaration x = Dangling $ "repeated declaration for: " ++ show x
redefinedBuiltin x = Dangling $ "redefined built-in method: " ++ show x
expressionIsNotLValue x = Dangling $ "expression is not an l-value: " ++ show x

-- Type resolution errors
notAComposedType typ = Dangling $ "expected composed type, got " ++ show typ
unknownMemberType typ x = Dangling $ concat ["unknown member type: ", show typ, " . ", show x]
unknownSymbolType x = Dangling $ "unknown symbol type: " ++ show x
subscriptNonArray = Dangling "not an array subscripted"
indexType = Dangling "invalid array index type"
unexpectedType expected actual = Dangling $ concat ["not matching type, expected: ", show expected, " actual ", show actual]
argumentsNotMatch expected actual = Dangling $ concat ["arguments do not match, expected: ", show expected, " actual: ", show actual]
badArithType = Dangling "bad operands type for arithmetic operator"
danglingReturn = Dangling "dangling return"
missingReturn = Dangling "missing return"
voidVar x = Dangling $ "a variable cannot have void type: " ++ show x
voidField x = Dangling $ "a field cannot have void type: " ++ show x
voidArg = Dangling "an argument cannot have void type"
voidNotIgnored = Dangling "void value not ignored as it should be"
incompatibleMain = Dangling "incompatible main() type"
missingMain = Dangling "missing main() function"
redeclaredType x = Dangling $ "redeclared type name: " ++ show x
referencedPrimitive typ = Dangling $ "primitive type cannot be referenced: " ++ show typ
danglingThis = Dangling "dangling this"
noSuperType typ = Dangling $ "cannot determine super type for: " ++ show typ
intValueOutOfBounds n = Dangling $ "int constant out of bounds: " ++ show n
uncaughtException typ = Dangling $ "uncaught, undeclared exception " ++ show typ

-- Runtime errors (not throwable by user)
noDefaultValue typ = Bare $ "type " ++ show typ ++ " has no default value"
nullPointerException = Bare "attempt to dereference null"
indexOutOfBounds x = Bare $ "index out of bounds: " ++ show x
zeroDivision = Bare "divided by zero"
intOverflow = Bare "integer overflow"
isConstant x = Bare $ "constant cannot be modified: " ++ show x

-- Terminal errors issued by a user
userIssuedError = Bare "runtime error"
uncaughtTopLevel x = Bare $ "uncaught top level exception: " ++ show x

-- INTERNAL ERRORS --
---------------------
unreachable ctx = error $ "unused pattern branch entered: " ++ show ctx
danglingReference loc = error $ "location: " ++ show loc ++ " does not exist"
fromJustFailure = error "fromJust failed"


module Jvmm.Builtins where

import Control.Monad.Identity
import Control.Monad.Error

import qualified Jvmm.Errors as Err
import Jvmm.Errors (ErrorInfoT, Location)
import Jvmm.Trans.Output
import qualified Jvmm.Scope as Scope

-- FUNCTIONS --
---------------
builtinFunctions :: [Method]
builtinFunctions = map fun [
    ("printInt", TypeMethod (TPrimitive TVoid) [TPrimitive TInt] []),
    ("readInt", TypeMethod (TPrimitive TInt) [] []),
    ("printString", TypeMethod (TPrimitive TVoid) [TComposed TString] []),
    ("readString", TypeMethod (TComposed TString) [] []),
    ("error", TypeMethod (TPrimitive TVoid) [] [])]
  where
    fun (name, typ) = Method typ (MethodName name) [] SBuiltin  TObject Err.Unknown []

-- ENTRYPOINT --
----------------
entrypointName = MethodName "main"
entrypointType = TypeMethod (TPrimitive TInt) [] []
isEntrypoint method = entrypointName == methodName method && methodOrigin method == TObject

-- TYPES --
-----------
isBuiltinType (TUser (ClassName str)) = str `elem` ["int", "char", "boolean", "string"]
isBuiltinType TObject = False
isBuiltinType (TArray _) = True
isBuiltinType _ = Err.unreachable TNull

builtinFieldType desc = case desc of
  (TArray _, "length") -> return $ toType TInt
  (TString, "length") -> return $ toType TInt
  _ -> throwError noMsg

builtinMethodType desc = case desc of
  (TString, "charAt") -> return $ toType $ TypeMethod (TPrimitive TChar) [TPrimitive TInt] []
  _ -> throwError noMsg


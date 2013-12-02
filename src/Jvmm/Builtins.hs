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
    ("printInt", TFunc TVoid [TInt] []),
    ("readInt", TFunc TInt [] []),
    ("printString", TFunc TVoid [TString] []),
    ("readString", TFunc TString [] []),
    ("error", TFunc TVoid [] [])]
  where
    fun (name, typ) = Method typ (MethodName name) [] SBuiltin TUnknown Err.Unknown []

-- ENTRYPOINT --
----------------
entrypointIdent = MethodName "main"
entrypointType = TFunc TInt [] []

-- TYPES --
-----------
isBuiltinType (ClassName str) = str `elem` ["int", "char", "boolean", "string"]

builtinFieldType typ uid = case (typ, uid) of
  (TArray _, FieldName "length$0") -> TInt
  (TString, FieldName "length$0") -> TInt
  _ -> TUnknown

builtinMethodType typ uid = case (typ, uid) of
  (TString, MethodName "charAt$0") -> TFunc TChar [TInt] []
  _ -> TUnknown

primitiveTypes = [TVoid, TInt, TChar, TBool]


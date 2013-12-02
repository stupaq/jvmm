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
entrypointIdent = MethodName "main"
entrypointType = TypeMethod (TPrimitive TInt) [] []

-- TYPES --
-----------
isBuiltinType (TUser (ClassName str)) = str `elem` ["int", "char", "boolean", "string"]
isBuiltinType TObject = False
isBuiltinType (TArray _) = True

builtinFieldType typ uid = case (typ, uid) of
  (TArray _, FieldName "length") -> TInt
  (TString, FieldName "length") -> TInt
  _ -> TUnknown

builtinMethodType typ uid = case (typ, uid) of
  (TString, MethodName "charAt") -> TypeMethod TChar [TInt] []
  _ -> TUnknown

primitiveTypes = [TVoid, TInt, TChar, TBool]


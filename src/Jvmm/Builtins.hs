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
    fun (name, typ) = Method typ (FIdent name) [] SBuiltin TUnknown Err.Unknown []

-- ENTRYPOINT --
----------------
entrypointIdent = Scope.tagGlobal $ FIdent "main"
entrypointType = TFunc TInt [] []

-- TYPES --
-----------
isBuiltinType typ = case typ of
  TUser (TIdent str) -> str `elem` ["int", "char", "boolean", "string"]
  _ -> False

builtinMemberType typ uid = case (typ, uid) of
  (TArray _, VIdent "length$0") -> TInt
  (TString, VIdent "length$0") -> TInt
  (TString, FIdent "charAt$0") -> TFunc TChar [TInt] []
  _ -> TUnknown

primitiveTypes = [TVoid, TInt, TChar, TBool]


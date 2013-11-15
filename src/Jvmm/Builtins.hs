module Jvmm.Builtins where

import Jvmm.Trans.Output
import qualified Jvmm.Scope as Scope

-- HIERARCHY --
---------------
buildObjectClass :: [Method] -> Class
buildObjectClass methods = Class {
    classType = TObject,
    classSuper = TUnknown,
    classFields = [],
    classMethods = builtins ++ methods
  }

-- FUNCTIONS --
---------------
builtins :: [Method]
builtins = map fun [
    ("printInt", TFunc TVoid [TInt] []),
    ("readInt", TFunc TInt [] []),
    ("printString", TFunc TVoid [TString] []),
    ("readString", TFunc TString [] []),
    ("error", TFunc TVoid [] [])]
  where
    fun (name, typ) = Method typ (FIdent name) [] SBuiltin TUnknown

-- ENTRYPOINT --
----------------
entrypointIdent = Scope.tagGlobal $ FIdent "main"
entrypointType = TFunc TInt [] []

-- TYPES --
-----------
isBuiltinType typ = case typ of
  TUser (TIdent str) -> str `elem` ["int", "char", "boolean", "string"]
  _ -> False

builtinMember typ uid = case (typ, uid) of
  (TArray _, VIdent "length$0") -> TInt
  (TString, VIdent "length$0") -> TInt
  (TString, FIdent "charAt$0") -> TFunc TChar [TInt] []
  _ -> TUnknown

primitiveTypes = [TVoid, TInt, TChar, TBool]


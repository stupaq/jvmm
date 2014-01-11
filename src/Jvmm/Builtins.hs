{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Jvmm.Builtins where

import Control.Monad.Error

import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output

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
    fun (name, typ) = Method typ (MethodName name) [] SBuiltin  TObject Err.Unknown [] False

isBuiltinFunction :: TypeComposed -> MethodName -> TypeMethod -> Bool
isBuiltinFunction TObject name typ = not $ Maybe.isNothing $ List.find builtin builtinFunctions
  where
    builtin method = methodName method == name && methodType method == typ
isBuiltinFunction _ _ _ = False

-- ENTRYPOINT --
----------------
entrypointType :: TypeMethod
entrypointType = TypeMethod (TPrimitive TInt) [] []

entrypointName :: MethodName
entrypointName = MethodName "main"

isEntrypoint :: Method -> Bool
isEntrypoint method =
  entrypointName == methodName method
  && methodOrigin method == TObject
  && not (methodInstance method)

-- TYPES --
-----------
isBuiltinType :: TypeComposed -> Bool
isBuiltinType (TUser (ClassName str)) = str `elem` ["int", "char", "boolean", "string"]
isBuiltinType TObject = False
isBuiltinType (TArray _) = True
isBuiltinType _ = Err.unreachable TNull

builtinFieldType :: forall (m :: * -> *) e. (MonadError e m, Error e) => (TypeComposed, String) -> m Type
builtinFieldType desc = case desc of
  (TArray _, "length") -> return $ toType TInt
  (TString, "length") -> return $ toType TInt
  _ -> throwError noMsg

builtinMethodType :: forall (m :: * -> *) e. (MonadError e m, Error e) => (TypeComposed, String) -> m Type
builtinMethodType desc = case desc of
  (TString, "charAt") -> return $ toType $ TypeMethod (TPrimitive TChar) [TPrimitive TInt] []
  _ -> throwError noMsg

defaultValue :: TypeBasic -> RValue
defaultValue typ = case typ of
  TComposed _ -> ENull
  TPrimitive TInt -> ELitInt 0
  TPrimitive TChar -> ELitChar '\0'
  TPrimitive TBool -> ELitFalse
  _ -> ENull


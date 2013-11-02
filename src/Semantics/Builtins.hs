module Semantics.Builtins where

import Prelude hiding (id)

import Semantics.APTree

buildObjectClass :: [Method] -> Class
buildObjectClass methods = Class {
    classType = TObject,
    classSuper = TUnknown,
    classFields = [],
    classMethods = builtins ++ methods
  }

builtins :: [Method]
builtins = map fun [
    ("printInt", TFunc TVoid [TInt] []),
    ("readInt", TFunc TInt [] []),
    ("printString", TFunc TVoid [TString] []),
    ("readString", TFunc TString [] []),
    ("error", TFunc TVoid [] [])]
  where
    fun (name, typ) = Method typ (FIdent name) [] SBuiltin


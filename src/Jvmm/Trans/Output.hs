{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Jvmm.Trans.Output where

import Control.Monad
import Control.Monad.Identity

import Jvmm.Errors (ErrorInfoT, runErrorInfoM)

-- This module provides internal representation of abstract syntax tree that
-- carries error reporting metadata, type information and many more.
-- We call this representation Abstract Program Tree to distinguish from the
-- ... that parser outputs.

-- UNIFIED IDENTIFIER --
------------------------
data UIdent =
    IThis
  | VIdent String
  | FIdent String
  | TIdent String
  deriving (Ord, Eq, Show)

(+/+) :: UIdent -> String -> UIdent
(VIdent id) +/+ str = VIdent $ id ++ str
(FIdent id) +/+ str = FIdent $ id ++ str
(TIdent id) +/+ str = TIdent $ id ++ str

(+/) :: UIdent -> Char -> UIdent
idt +/ char = case idt of
  (VIdent id) -> VIdent $ strip id
  (FIdent id) -> FIdent $ strip id
  (TIdent id) -> TIdent $ strip id
  where
    strip :: String -> String
    strip = takeWhile (/= char)

-- COMPILATION UNIT --
----------------------
data CompilationUnit =
  -- Type is a superclass,
  -- ClassDiff maps superclass into class
  CompilationUnit [(Type, ClassDiff)]
  deriving (Show)

-- CLASS --
-----------
data Class = Class {
    classType :: Type
  , classSuper :: Type
  , classFields :: [Field]
  , classMethods :: [Method]
} deriving (Eq, Ord, Show)

type ClassDiff = Class -> ErrorInfoT Identity Class

instance Show ClassDiff where
  show diff = show $ runErrorInfoM $ diff Class {
        classType = TUnknown
      , classSuper = TUnknown
      , classFields = []
      , classMethods = []
    }

data Field = Field {
    fieldType :: Type
  , fieldIdent :: UIdent
  , fieldOrigin :: Type
} deriving (Eq, Ord, Show)

data Method = Method {
    methodType :: Type
  , methodIdent :: UIdent
  , methodArgs :: [UIdent]
  , methodBody :: Stmt
  , methodOrigin :: Type
} deriving (Eq, Ord, Show)

-- STATEMENTS --
----------------
data Stmt =
   SLocal [Variable] [Stmt]
 | SDeclVar Type UIdent
 | SEmpty
 | SAssign UIdent Expr
 | SAssignArr UIdent Expr Expr
 | SAssignFld UIdent UIdent Expr
 | SReturn Expr
 | SReturnV
 | SIf Expr Stmt
 | SIfElse Expr Stmt Stmt
 | SWhile Expr Stmt
 | SExpr Expr
 | SThrow Expr
 | STryCatch Stmt Type UIdent Stmt
 | SBuiltin
 | SInherited
  deriving (Eq, Ord, Show)

data Variable =
  Variable Type UIdent
  deriving (Eq, Ord, Show)

-- TYPES --
-----------
data Type =
   TFunc Type [Type] [Type]
 | TUnknown
 | TNull
 | TVoid
 | TInt
 | TChar
 | TBool
 | TString
 | TObject
 | TUser UIdent
 | TArray Type
  deriving (Eq,Ord,Show)

-- EXPRESSIONS --
-----------------
data Expr =
   EBinary Type OpBin Expr Expr
 | EUnary Type OpUn Expr
 | EVar UIdent
 | ELitInt Integer
 | ELitTrue
 | ELitFalse
 | ELitString String
 | ELitChar Char
 | ENull
 | EAccessArr Expr Expr
 | EAccessFn Expr UIdent [Expr]
 | EAccessVar Expr UIdent
 | ENewObj Type
 | ENewArr Type Expr
 | EThis
  deriving (Eq,Ord,Show)

-- BINARY OPERATIONS --
-----------------------
data OpUn =
   OuNeg
 | OuNot
  deriving (Eq,Ord,Show)

-- UNARY OPERATIONS --
----------------------
data OpBin =
   ObTimes
 | ObDiv
 | ObMod
 | ObPlus
 | ObMinus
 | ObLTH
 | ObLEQ
 | ObGTH
 | ObGEQ
 | ObEQU
 | ObNEQ
 | ObAnd
 | ObOr
  deriving (Eq,Ord,Show)


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Semantics.APTree where

import Prelude hiding (id)
import Control.Monad
import Control.Monad.Identity
import Data.List (partition, find)

import Semantics.Commons
import Semantics.Errors (ErrorInfoT, runErrorInfoM)

-- This module provides internal representation of abstract syntax tree that
-- carries error reporting metadata, type information and many more.
-- We call this representation Abstract Program Tree to distinguish from the
-- shit that parser outputs..

-- UNIFIED IDENTIFIER --
------------------------
data UIdent =
    VIdent String
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

-- PROGRAM --
-------------
data CompilationUnit =
  -- Type is a superclass,
  -- ClassDiff maps superclass into class
  CompilationUnit [(Type, ClassDiff)]
  deriving (Show)

-- CLASS HIERARCHY --
---------------------
type ClassHierarchy = Hierarchy Class

data Class = Class {
  classType :: Type,
  classSuper :: Type,
  classFields :: [Field],
  classMethods :: [Method]
} deriving (Eq, Ord, Show)

type ClassDiff = Class -> ErrorInfoT Identity Class

-- TODO this is only for debug
instance Show ClassDiff where
  show diff = show $ runErrorInfoM $ diff Class {
      classType = TUnknown,
      classSuper = TUnknown,
      classFields = [],
      classMethods = []
    }

data Field =
  Field Type UIdent
  deriving (Eq, Ord, Show)

data Method =
  Method Type UIdent [UIdent] Stmt
  deriving (Eq, Ord, Show)

-- STATEMENTS --
----------------
data Stmt =
   SLocal [Variable] [Stmt]
 | SDeclVar Type UIdent
 | SEmpty
 | SAssign UIdent Expr
 | SAssignArr UIdent Expr Expr
 | SAssignFld Expr UIdent Expr
 | SReturn Expr
 | SReturnV
 | SIf Expr Stmt
 | SIfElse Expr Stmt Stmt
 | SWhile Expr Stmt
 | SExpr Expr
 | SThrow Expr
 | STryCatch Stmt Type UIdent Stmt
 | SBuiltin
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
 | EApp UIdent [Expr]
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


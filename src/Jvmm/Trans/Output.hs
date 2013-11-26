{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Jvmm.Trans.Output where

import Control.Monad.Identity
import Control.Monad.Error

import qualified Jvmm.Errors as Err
import Jvmm.Errors (ErrorInfo, ErrorInfoT, runErrorInfoM, withLocation, Location)

-- This module provides internal representation of abstract syntax tree that
-- carries error reporting metadata, type information and many more.
-- We call this representation Abstract Program Tree to distinguish from the
-- ... that parser outputs.

-- CLASS --
-----------
data Class = Class {
    className :: ClassName
  , classSuper :: Type
  , classFields :: [Field]
  , classMethods :: [Method]
  , classStaticMethods :: [Method]
  , classLocation :: Location
} deriving (Eq, Ord, Show)

classType :: Class -> Type
classType = TUser . className

newtype ClassName = ClassName String
  deriving (Show, Eq, Ord)

data Field = Field {
    fieldType :: Type
  , fieldName :: FieldName
  , fieldOrigin :: Type
} deriving (Eq, Ord, Show)

newtype FieldName = FieldName String
  deriving (Show, Eq, Ord)

fieldFromVariable :: VariableName -> FieldName
fieldFromVariable (VariableName name) = FieldName name

data Method = Method {
    methodType :: Type
  , methodName :: MethodName
  , methodArgs :: [VariableName]
  , methodBody :: Stmt
  , methodOrigin :: Type
  , methodLocation :: Location
  , methodVariables :: [Variable]
} deriving (Eq, Ord, Show)

newtype MethodName = MethodName String
  deriving (Show, Eq, Ord)

data Variable = Variable {
    variableType :: Type
  , variableNum :: VariableNum
  , variableName :: VariableName
} deriving (Eq, Ord, Show)

type VariableNum = Int
variablenum0 = 0 :: VariableNum

newtype VariableName = VariableName String
  deriving (Show, Eq, Ord)

-- STATEMENTS --
----------------
data Stmt =
    SEmpty
  | SStore VariableNum Expr
  | SStoreArray VariableNum Expr Expr
  | SPutField VariableNum FieldName Expr
  | SExpr Expr
  | SBlock [Stmt]
  -- Control statements
  | SReturn Expr
  | SReturnV
  | SIf Expr Stmt
  | SIfElse Expr Stmt Stmt
  | SWhile Expr Stmt
  | SThrow Expr
  | STryCatch Stmt Type VariableNum Stmt
  -- Special function bodies
  | SBuiltin
  | SInherited
  -- Metainformation carriers
  | SMetaLocation Location [Stmt]
  -- These statements will be replaced with ones caring more context in subsequent phases
  | T_SDeclVar Type VariableName
  | T_SAssign VariableName Expr
  | T_SAssignArr VariableName Expr Expr
  | T_SAssignFld VariableName FieldName Expr
  deriving (Eq, Ord, Show)

-- TYPES --
-----------
data Type =
    TUnknown
  | TFunc Type [Type] [Type]
  | TNull
  | TVoid
  | TInt
  | TChar
  | TBool
  | TString
  | TObject
  | TUser ClassName
  | TArray Type
  deriving (Eq,Ord,Show)

-- EXPRESSIONS --
-----------------
data Expr =
    ENull
  | ELoad VariableNum
  | ELoadThis
  | EArrayLoad Expr Expr
  | EGetField Expr FieldName
  | EInvokeStatic MethodName [Expr]
  | EInvokeVirtual Expr MethodName [Expr]
  | ELitTrue
  | ELitFalse
  | ELitChar Char
  | ELitString String
  | ELitInt Integer
  | ENewObj Type
  | ENewArr Type Expr
  | EBinary Type OpBin Expr Expr
  | EUnary Type OpUn Expr
  -- These expressions will be replaced with ones caring more context in subsequent phases
  | T_EVar VariableName
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

-- AUXILIARY --
---------------
stmtMetaLocation :: (MonadError ErrorInfo m) => Location -> m [Stmt] -> m Stmt
stmtMetaLocation loc action = do
  stmts' <- withLocation loc action
  return $ SMetaLocation loc stmts'

-- TOP LEVEL --
---------------
data CompilationUnit =
  -- Type is a superclass,
  -- ClassDiff maps superclass into class
  CompilationUnit [(Type, ClassDiff)]
  deriving (Show)

type ClassDiff = Class -> ErrorInfoT Identity Class

instance Show ClassDiff where
  show diff = show $ runErrorInfoM $ diff Class {
        className = ClassName ""
      , classSuper = TUnknown
      , classFields = []
      , classMethods = []
      , classStaticMethods = []
      , classLocation = Err.Unknown
    }


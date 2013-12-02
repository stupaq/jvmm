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
    classType :: Type
  , classSuper :: Type
  , classFields :: [Field]
  , classMethods :: [Method]
  , classStaticMethods :: [Method]
  , classLocation :: Location
} deriving (Eq, Ord, Show)

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
  , methodArgs :: [Variable]
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
variablenumThis = 0 :: VariableNum
variablenumNone = -1 :: VariableNum

newtype VariableName = VariableName String
  deriving (Show, Eq, Ord)
variablename0 = VariableName "unknown-variable-name"

-- STATEMENTS --
----------------
data Stmt =
    SEmpty
  | SBlock [Stmt]
  | SExpr Expr
  -- Memory access
  | SStore VariableNum Expr
  | SStoreArray VariableNum Expr Expr
  | SPutField VariableNum FieldName Expr
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
  | T_STryCatch Stmt Type VariableName Stmt
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

newtype ClassName = ClassName String
  deriving (Show, Eq, Ord)

-- EXPRESSIONS --
-----------------
data Expr =
  -- Literals
    ENull
  | ELitTrue
  | ELitFalse
  | ELitChar Char
  | ELitString String
  | ELitInt Integer
  -- Memory access
  | ELoad VariableNum
  | EArrayLoad Expr Expr
  | EGetField Expr FieldName
  -- Method calls
  | EInvokeStatic MethodName [Expr]
  | EInvokeVirtual Expr MethodName [Expr]
  -- Object creation
  | ENewObj Type
  | ENewArr Type Expr
  -- Operations
  | EUnary Type OpUn Expr
  | EBinary Type OpBin Expr Expr
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
        classType = TUnknown
      , classSuper = TUnknown
      , classFields = []
      , classMethods = []
      , classStaticMethods = []
      , classLocation = Err.Unknown
    }


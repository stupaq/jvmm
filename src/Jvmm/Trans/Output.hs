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
    classType :: TypeComposed
  , classSuper :: TypeComposed
  , classFields :: [Field]
  , classMethods :: [Method]
  , classStaticMethods :: [Method]
  , classLocation :: Location
} deriving (Show, Eq, Ord)

data Field = Field {
    fieldType :: TypeBasic
  , fieldName :: FieldName
  , fieldOrigin :: TypeComposed
} deriving (Show, Eq, Ord)

newtype FieldName = FieldName String
  deriving (Show, Eq, Ord)

fieldFromVariable :: VariableName -> FieldName
fieldFromVariable (VariableName name) = FieldName name

data Method = Method {
    methodType :: TypeMethod
  , methodName :: MethodName
  , methodArgs :: [Variable]
  , methodBody :: Stmt
  , methodOrigin :: TypeComposed
  , methodLocation :: Location
  , methodVariables :: [Variable]
} deriving (Eq, Ord, Show)

newtype MethodName = MethodName String
  deriving (Show, Eq, Ord)

data Variable = Variable {
    variableType :: TypeBasic
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
  | STryCatch Stmt TypeComposed VariableNum Stmt
  -- Special function bodies
  | SBuiltin
  | SInherited
  -- Metainformation carriers
  | SMetaLocation Location [Stmt]
  -- These statements will be replaced with ones caring more context in subsequent phases
  | T_SDeclVar TypeBasic VariableName
  | T_SAssign VariableName Expr
  | T_SAssignArr VariableName Expr Expr
  | T_SAssignFld VariableName FieldName Expr
  | T_STryCatch Stmt TypeComposed VariableName Stmt
  deriving (Eq, Ord, Show)

-- TYPES --
-----------
data Type =
    TUnknown
  | TMethod TypeMethod
  | TBasic TypeBasic
  deriving (Show, Eq, Ord)

data TypeMethod = TypeMethod TypeBasic [TypeBasic] [TypeComposed]
  deriving (Show, Eq, Ord)

data TypeBasic =
    TPrimitive TypePrimitive
  | TComposed TypeComposed
  deriving (Show, Eq, Ord)

data TypePrimitive =
    TVoid
  | TInt
  | TChar
  | TBool
  deriving (Show, Eq, Ord)

data TypeComposed =
    TNull
  | TString
  | TArray TypeBasic
  | TObject
  | TUser ClassName
  deriving (Show, Eq, Ord)

newtype ClassName = ClassName String
  deriving (Show, Eq, Ord)

class InheritsType a where
  toType :: a -> Type
instance InheritsType Type where
  toType = Prelude.id
instance InheritsType TypeMethod where
  toType = TMethod
instance InheritsType TypeBasic where
  toType = TBasic
instance InheritsType TypePrimitive where
  toType = toType . TPrimitive
instance InheritsType TypeComposed where
  toType = toType . TComposed

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
  | ENewObj TypeBasic
  | ENewArr TypeBasic Expr
  -- Operations
  | EUnary TypeBasic OpUn Expr
  | EBinary TypeBasic OpBin Expr Expr
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

-- TODO this is for debug purpouses only, discard when no longer needed
instance Show ClassDiff where
  show diff = show $ runErrorInfoM $ diff Class {
        classType = TObject
      , classSuper = TObject
      , classFields = []
      , classMethods = []
      , classStaticMethods = []
      , classLocation = Err.Unknown
    }


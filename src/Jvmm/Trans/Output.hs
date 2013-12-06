{-# LANGUAGE MultiParamTypeClasses #-}
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
} deriving (Show)

data Field = Field {
    fieldType :: TypeBasic
  , fieldName :: FieldName
  , fieldOrigin :: TypeComposed
} deriving (Show)

data Method = Method {
    methodType :: TypeMethod
  , methodName :: MethodName
  , methodArgs :: [Variable]
  , methodBody :: Stmt
  , methodOrigin :: TypeComposed
  , methodLocation :: Location
  , methodVariables :: [Variable]
} deriving (Show)

data Variable = Variable {
    variableType :: TypeBasic
  , variableNum :: VariableNum
  , variableName :: VariableName
} deriving (Show)

-- NAMES --
-----------
data VariableNum =
    VariableNum Int
  | VariableThis
  deriving (Show, Eq, Ord)

instance Enum VariableNum where
  fromEnum VariableThis = 0
  fromEnum (VariableNum num) = num
  toEnum num
    | num == 0 = VariableThis
    | otherwise = VariableNum num

newtype VariableName = VariableName String
  deriving (Show, Eq, Ord)

newtype FieldName = FieldName String
  deriving (Show, Eq, Ord)

fieldFromVariable :: VariableName -> FieldName
fieldFromVariable (VariableName name) = FieldName name

newtype MethodName = MethodName String
  deriving (Show, Eq, Ord)

-- STATEMENTS --
----------------
data Stmt =
    SEmpty
  | SBlock [Stmt]
  | SExpr Expr
  -- Memory access
  | SStore VariableNum Expr TypeBasic
  | SStoreArray VariableNum Expr Expr TypeBasic
  | SPutField VariableNum TypeComposed FieldName Expr TypeBasic
  -- Control statements
  | SReturn Expr TypeBasic
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
  deriving (Show)

-- TYPES --
-----------
data Type =
    TMethod TypeMethod
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
  | ELoad VariableNum TypeBasic
  | EArrayLoad Expr Expr TypeBasic
  | EGetField Expr TypeComposed FieldName TypeBasic
  -- Method calls
  | EInvokeStatic TypeComposed MethodName [Expr]
  | EInvokeVirtual Expr TypeComposed MethodName [Expr]
  -- Object creation
  | ENewObj TypeComposed
  | ENewArr TypeBasic Expr
  -- Operations
  | EUnary OpUn Expr TypeBasic
  | EBinary OpBin Expr Expr TypeBasic
  -- These expressions will be replaced with ones caring more context in subsequent phases
  | T_EVar VariableName
  deriving (Show)

-- BINARY OPERATIONS --
-----------------------
data OpUn =
   OuNeg
 | OuNot
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

-- AUXILIARY --
---------------
stmtMetaLocation :: (MonadError ErrorInfo m) => Location -> m [Stmt] -> m Stmt
stmtMetaLocation loc action = do
  stmts' <- withLocation loc action
  return $ SMetaLocation loc stmts'

stmtMetaLocation' :: (MonadError ErrorInfo m) => Location -> m [a] -> m ()
stmtMetaLocation' loc action = stmtMetaLocation loc (action >> return []) >> return ()

-- TOP LEVEL --
---------------
data CompilationUnit =
  -- Type is a superclass,
  -- ClassDiff maps superclass into class
  CompilationUnit [(TypeComposed, ClassDiff)]
  deriving (Show)

type ClassDiff = Class -> ErrorInfoT Identity Class

instance Show ClassDiff where
  show diff = show $ runErrorInfoM $ diff Class {
      -- Since we are way before resolving inheritance hierarchy here,
      -- we are not aware of some class' traits.
        classType = TObject
      , classSuper = TObject
      , classFields = []
      , classMethods = []
      , classStaticMethods = []
      , classLocation = Err.Unknown
    }


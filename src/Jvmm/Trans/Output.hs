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

uidentName :: UIdent -> String
uidentName x = case x of
  VIdent str -> str
  FIdent str -> str
  TIdent str -> str
  IThis -> "self"

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

newtype ClassName = ClassName String
  deriving (Show, Eq, Ord)

data Field = Field {
    fieldType :: Type
  , fieldIdent :: UIdent
  , fieldOrigin :: Type
} deriving (Eq, Ord, Show)

newtype FieldName = FieldName String
  deriving (Show, Eq, Ord)

data Method = Method {
    methodType :: Type
  , methodIdent :: UIdent
  , methodArgs :: [UIdent]
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

newtype VariableName = VariableName String
  deriving (Show, Eq, Ord)

-- STATEMENTS --
----------------
data Stmt =
    SEmpty
  | SStore VariableNum Expr
  | SStoreArray VariableNum Expr
  | SPutField VariableNum UIdent Expr
  | SExpr Expr
  | SBlock [Stmt]
  -- Control statements
  | SReturn Expr
  | SReturnV
  | SIf Expr Stmt
  | SIfElse Expr Stmt Stmt
  | SWhile Expr Stmt
  | SThrow Expr
  | STryCatch Stmt Type UIdent Stmt
  -- Special function bodies
  | SBuiltin
  | SInherited
  -- Metainformation carriers
  | SMetaLocation Location [Stmt]
  -- These statements will be replaced with ones caring more context in subsequent phases
  | T_SDeclVar Type UIdent
  | T_SAssign UIdent Expr
  | T_SAssignArr UIdent Expr Expr
  | T_SAssignFld UIdent UIdent Expr
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
  | TUser UIdent
  | TArray Type
  deriving (Eq,Ord,Show)

-- EXPRESSIONS --
-----------------
data Expr =
    ENull
  | ELoad VariableNum
  | ELoadThis
  | EArrayLoad Expr Expr
  | EGetField Expr UIdent
  | ELitTrue
  | ELitFalse
  | ELitChar Char
  | ELitString String
  | ELitInt Integer
  | ECall UIdent [Expr]
  | EAccessFn Expr UIdent [Expr]
  | ENewObj Type
  | ENewArr Type Expr
  | EBinary Type OpBin Expr Expr
  | EUnary Type OpUn Expr
  -- These expressions will be replaced with ones caring more context in subsequent phases
  | T_EVar UIdent
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


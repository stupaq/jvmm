module Semantics.APTree where
import Prelude hiding (id)
import Data.List (partition, find)

-- This module provides internal representation of abstract syntax tree that
-- carries error reporting metadata, type information and many more.
-- We call this representation Abstract Program Tree to distinguish from the
-- shit that parser outputs..

-- FIXME rename UIdent to Ident

-- Unified identifier
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


data Stmt =
   SLocal [Stmt] [Stmt]
 | SGlobal [Stmt]
 | SDefClass UIdent Stmt
 | SDefFunc Type UIdent [Stmt] [Type] Stmt
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
  deriving (Eq,Ord,Show)


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
  deriving (Eq,Ord,Show)


data OpUn =
   OuNeg
 | OuNot
  deriving (Eq,Ord,Show)


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


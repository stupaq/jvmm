module Syntax.AbsJvmm where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
newtype Semicolon = Semicolon ((Int,Int),String) deriving (Eq,Ord,Show)
data Declaration =
   DVariable Type Ident
  deriving (Eq,Ord,Show)

data Program =
   Program [Definition]
  deriving (Eq,Ord,Show)

data Definition =
   DFunction Function
 | DClass Class
  deriving (Eq,Ord,Show)

data Function =
   Function Type Ident [Argument] Exceptions [Stmt]
  deriving (Eq,Ord,Show)

data Argument =
   Argument Type Ident
  deriving (Eq,Ord,Show)

data Exceptions =
   NoExceptions
 | Exceptions [Type]
  deriving (Eq,Ord,Show)

data Class =
   Class Ident Extends [Member]
  deriving (Eq,Ord,Show)

data Member =
   Field Declaration Semicolon
 | Method Function
  deriving (Eq,Ord,Show)

data Extends =
   SuperClass Type
 | SuperObject
  deriving (Eq,Ord,Show)

data Type =
   TObject
 | TUser Ident
 | TArray Type
 | TChar
 | TInt
 | TString
 | TBool
 | TVoid
  deriving (Eq,Ord,Show)

data Stmt =
   SThrow Expr Semicolon
 | STryCatch Stmt Type Ident Stmt
 | SBlock [Stmt]
 | SEmpty Semicolon
 | SDeclVar Type [Item] Semicolon
 | SAssign Ident Expr Semicolon
 | SAssignArr Ident Expr Expr Semicolon
 | SAssignFld Ident Ident Expr Semicolon
 | SAssignThis Ident Expr Semicolon
 | SPostInc Ident Semicolon
 | SPostDec Ident Semicolon
 | SAssignOp Ident AssignOp Expr Semicolon
 | SAssignOpArr Ident Expr AssignOp Expr Semicolon
 | SAssignOpFld Ident Ident AssignOp Expr Semicolon
 | SAssignOpThis Ident AssignOp Expr Semicolon
 | SReturn Expr Semicolon
 | SReturnV Semicolon
 | SIf Expr Stmt
 | SIfElse Expr Stmt Stmt
 | SWhile Expr Stmt
 | SForeach Type Ident Expr Stmt
 | SExpr Expr Semicolon
  deriving (Eq,Ord,Show)

data Item =
   NoInit Ident
 | Init Ident Expr
  deriving (Eq,Ord,Show)

data Expr =
   EArrayE Expr Expr
 | EMethodE Expr Ident [Expr]
 | EFieldE Expr Ident
 | EArrayI Ident Expr
 | EMethodI Ident Ident [Expr]
 | EFieldI Ident Ident
 | ENewObject Type
 | ENewArray Type Expr
 | EMethodIT Ident [Expr]
 | EFieldIT Ident
 | EThis
 | ENullT Type
 | ENull
 | ELitChar Char
 | EVar Ident
 | ELitInt Integer
 | ELitTrue
 | ELitFalse
 | EApp Ident [Expr]
 | EString String
 | ENeg Expr
 | ENot Expr
 | EMul Expr MulOp Expr
 | EAdd Expr AddOp Expr
 | ERel Expr RelOp Expr
 | EAnd Expr Expr
 | EOr Expr Expr
  deriving (Eq,Ord,Show)

data AssignOp =
   APlus
 | AMinus
 | ATimes
 | ADiv
 | AMod
  deriving (Eq,Ord,Show)

data AddOp =
   Plus
 | Minus
  deriving (Eq,Ord,Show)

data MulOp =
   Times
 | Div
 | Mod
  deriving (Eq,Ord,Show)

data RelOp =
   LTH
 | LEQ
 | GTH
 | GEQ
 | EQU
 | NEQ
  deriving (Eq,Ord,Show)


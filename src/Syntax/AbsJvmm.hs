module Syntax.AbsJvmm where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
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

data Exceptions =
   NoExceptions
 | Exceptions [Type]
  deriving (Eq,Ord,Show)

data Argument =
   Argument Type Ident
  deriving (Eq,Ord,Show)

data Class =
   Class Ident Extends [Member]
  deriving (Eq,Ord,Show)

data Member =
   Field Declaration
 | Method Function
  deriving (Eq,Ord,Show)

data Extends =
   SuperClass Type
 | SuperObject
  deriving (Eq,Ord,Show)

data Type =
   TVoid
 | TInt
 | TChar
 | TBool
 | TString
 | TObject
 | TUser Ident
 | TArray Type
  deriving (Eq,Ord,Show)

data Stmt =
   SBlock [Stmt]
 | SAssignOp Ident AssignOp Expr
 | SAssignOpArr Ident Expr AssignOp Expr
 | SAssignOpFld Ident Ident AssignOp Expr
 | SAssignOpThis Ident AssignOp Expr
 | SPostInc Ident
 | SPostDec Ident
 | SEmpty
 | SAssign Ident Expr
 | SAssignArr Ident Expr Expr
 | SAssignFld Ident Ident Expr
 | SAssignThis Ident Expr
 | SReturn Expr
 | SReturnV
 | SIf Expr Stmt
 | SIfElse Expr Stmt Stmt
 | SWhile Expr Stmt
 | SForeach Type Ident Expr Stmt
 | SExpr Expr
 | SThrow Expr
 | STryCatch Stmt Type Ident Stmt
 | SDeclVar Type [Item]
  deriving (Eq,Ord,Show)

data Item =
   NoInit Ident
 | Init Ident Expr
  deriving (Eq,Ord,Show)

data Expr =
   EArray Expr Expr
 | EMethod Expr Ident [Expr]
 | EField Expr Ident
 | EArrayI Ident Expr
 | EMethodI Ident Ident [Expr]
 | EFieldI Ident Ident
 | ENewObject Type
 | ENewArray Type Expr
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


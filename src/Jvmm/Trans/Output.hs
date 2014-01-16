{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TypeSynonymInstances     #-}
module Jvmm.Trans.Output where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity

import Data.List as List
import Data.Tree as Tree

import Jvmm.Errors (ErrorInfo, ErrorInfoT, Location, runErrorInfoM, withLocation)
import qualified Jvmm.Errors as Err

-- COMPILATION UNIT --
----------------------
data CompilationUnit =
    -- Convention: (super type, constructs class from super class)
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
      , classAllMethods = []
      , classLocation = Err.Unknown
    }

-- CLASS HIERARCHY --
---------------------
type ClassHierarchy = Tree.Tree Class

-- CLASS --
-----------
data Class = Class {
    classType       :: TypeComposed
  , classSuper      :: TypeComposed
  , classFields     :: [Field]
  , classAllMethods :: [Method]
  , classLocation   :: Location
} deriving (Show)

classStaticMethods, classInstanceMethods :: Class -> [Method]
classStaticMethods = List.filter (not . methodInstance) . classAllMethods
classInstanceMethods = List.filter methodInstance . classAllMethods

data Field = Field {
    fieldType   :: TypeBasic
  , fieldName   :: FieldName
  , fieldOrigin :: TypeComposed
} deriving (Show)

data Method = Method {
    methodType      :: TypeMethod
  , methodName      :: MethodName
  , methodArgs      :: [Variable]
  , methodBody      :: Stmt
  , methodOrigin    :: TypeComposed
  , methodLocation  :: Location
  , methodVariables :: [Variable]
  , methodInstance  :: Bool
} deriving (Show)

data Variable = Variable {
    variableType :: TypeBasic
  , variableNum  :: VariableNum
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

newtype MethodName = MethodName { string :: String }
  deriving (Show, Eq, Ord)

-- STATEMENTS --
----------------
data Stmt =
    SEmpty
  | SBlock [Stmt]
  | SExpr RValue TypeBasic
  -- Memory access
  | SAssign LValue RValue TypeBasic
  -- Control statements
  | SReturn RValue TypeBasic
  | SReturnV
  | SIf RValue Stmt
  | SIfElse RValue Stmt Stmt
  | SWhile RValue Stmt
  | SThrow RValue
  | STryCatch Stmt TypeComposed VariableNum Stmt
  -- Special function bodies
  | SBuiltin
  | SInherited
  -- Metainformation carriers
  | SMetaLocation Location [Stmt]
  -- These statements will be replaced with ones caring more context in subsequent phases
  | PruneSDeclVar TypeBasic VariableName
  | PruneSTryCatch Stmt TypeComposed VariableName Stmt
  deriving (Show)

-- EXPRESSIONS --
-----------------
data RValue =
  -- Literals
    ENull TypeComposed
  | ELitTrue
  | ELitFalse
  | ELitChar Char
  | ELitString String
  | ELitInt Integer
  -- Memory access
  | ELoad VariableNum TypeBasic
  | EArrayLoad RValue RValue TypeBasic
  | EGetField RValue TypeComposed FieldName TypeBasic
  -- Method calls
  | EInvokeStatic TypeComposed MethodName TypeMethod [RValue]
  | EInvokeVirtual RValue TypeComposed MethodName TypeMethod [RValue]
  -- Object creation
  | ENewObj TypeComposed
  | ENewArr TypeBasic RValue
  -- Operations
  | EUnary OpUn RValue TypeBasic
  | EBinary OpBin RValue RValue TypeBasic TypeBasic TypeBasic
  -- These expressions will be replaced with ones caring more context in subsequent phases
  | PruneEVar VariableName
  | PruneENull
  deriving (Show, Eq)

isLiteral :: RValue -> Bool
isLiteral x = case x of
  ENull _ -> True
  ELitTrue -> True
  ELitFalse -> True
  ELitChar _ -> True
  ELitString _ -> True
  ELitInt _ -> True
  _ -> False

data LValue =
    LVariable VariableNum TypeBasic
  | LArrayElement LValue RValue TypeBasic
  | LField LValue TypeComposed FieldName TypeBasic
  -- These expressions will be replaced with ones caring more context in subsequent phases
  | PruneLExpr RValue
  deriving (Show)

class InheritsRValue a where
  toRValue :: a -> RValue
instance InheritsRValue RValue where
  toRValue = Prelude.id
instance InheritsRValue LValue where
  toRValue (LVariable num typ) = ELoad num typ
  toRValue (LArrayElement lval expr typ) = EArrayLoad (toRValue lval) expr typ
  toRValue (LField lval ctyp name typ) = EGetField (toRValue lval) ctyp name typ
  toRValue (PruneLExpr expr) = expr

class InheritsLValue a where
  toLValue :: a -> LValue
  isPureLValue :: a -> Bool
  isPureLValue x = case toLValue x of
    LVariable _ _ -> True
    LArrayElement lval _ _ -> isPureLValue lval
    LField lval _ _ _ -> isPureLValue lval
    PruneLExpr _ -> False
instance InheritsLValue LValue where
  toLValue = Prelude.id
instance InheritsLValue RValue where
  toLValue (ELoad num typ) = LVariable num typ
  toLValue (EArrayLoad rval expr2 typ) = LArrayElement (toLValue rval) expr2 typ
  toLValue (EGetField rval ctyp name typ) = LField (toLValue rval) ctyp name typ
  toLValue expr = PruneLExpr expr

-- OPERATIONS --
----------------
data OpUn =
   OuNeg
 | OuNot
  deriving (Show, Eq, Ord)

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

-- TYPES --
-----------
data Type =
    TMethod TypeMethod
  | TBasic TypeBasic
  deriving (Show, Eq, Ord)

data TypeMethod = TypeMethod {
    typemethodReturn :: TypeBasic
  , typemethodArgs   :: [TypeBasic]
  , typemethodThrows :: [TypeComposed]
} deriving (Show, Eq, Ord)

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
    TString
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

-- AUXILIARY --
---------------
stmtMetaLocation :: (MonadError ErrorInfo m) => Location -> m [Stmt] -> m Stmt
stmtMetaLocation loc action = do
  stmts' <- withLocation loc action
  return $ SMetaLocation loc stmts'

stmtMetaLocation' :: (MonadError ErrorInfo m, Functor m) => Location -> m [a] -> m ()
stmtMetaLocation' loc action = void $ stmtMetaLocation loc (action >> return [])

class IncrementalAttribute a where
  undef :: a
  undef = error "value of this field is not defined at this stage of compilation"

instance IncrementalAttribute TypeBasic where

instance IncrementalAttribute TypeComposed where

instance IncrementalAttribute TypeMethod where

instance IncrementalAttribute VariableNum where

-- PORN --
----------
infixl 4 <#>
(<#>) :: forall (f :: * -> *) a b. Applicative f => f (a -> b) -> a -> f b
(<#>) x y = x <*> pure y


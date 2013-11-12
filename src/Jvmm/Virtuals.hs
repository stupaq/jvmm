module Jvmm.Virtuals (virtuals) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Traversable as Traversable

import Semantics.Commons
import qualified Semantics.Errors as Err
import Semantics.Builtins
import Semantics.Errors (rethrow, ErrorInfoT, runErrorInfoM)
import Semantics.APTree
import qualified Jvmm.Scope as Scope

-- EXECUTION UNIT --
--------------------
data ExecutionUnit = ExecutionUnit {
    unitFuncs :: [Function]
  , unitStructs :: Map.Map Type Structure
  , unitDebug :: DebugSymbols
} deriving (Show)

data DebugSymbols = DebugSymbols {
    debugFuncs :: Map.Map (Type, Offset) UIdent
  , debugFields :: Map.Map (Type, Offset) UIdent
} deriving (Show)

data Function = Function {
    functionDescr :: Descriptor
  , functionArgs :: [UIdent]
  , functionStatic :: Bool
  , functionBody :: Stmt
} deriving (Show)

data Structure = Structure {
    structureVtable :: VTable
  , structureFields :: Map.Map UIdent Offset
  , structureMethods :: Map.Map UIdent Offset
} deriving (Show)

type Offset = Int

-- VTABLE REPRESENTATION --
---------------------------
type VTable = Map.Map Offset Descriptor

type Descriptor = String
descriptorDelimiter = '/'

descript :: Type -> UIdent -> VirtualsM Descriptor
descript typ uid = case (typ, uid) of
  (TUnknown, FIdent method) -> return $ descriptorDelimiter:method
  (TUser (TIdent origin), FIdent method) -> return $ origin ++ (descriptorDelimiter:method)
  _ -> throwError $ Err.unusedBranch (typ, uid)

-- VIRTUALS MONAD --
--------------------
type VirtualsM = WriterT [(Type, Structure)] (WriterT [Function] (ErrorInfoT Identity))
runVirtualsM :: VirtualsM a -> ErrorInfoT Identity ((a, [(Type, Structure)]), [Function])
runVirtualsM = runWriterT . runWriterT

-- VIRTUAL BINDING PREPARATION --
---------------------------------
virtuals :: ClassHierarchy -> ErrorInfoT Identity ExecutionUnit
virtuals = undefined


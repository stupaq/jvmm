{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Jvmm.LlvmEmitter.Internal where
import Jvmm.LlvmEmitter.Layout
import Jvmm.LlvmEmitter.Output

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Map as Map
import qualified Data.Traversable as Traversable

import Jvmm.Builtins
import Jvmm.Errors (ErrorInfoT)
import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output

import LLVM.General.AST as Llvm
import LLVM.General.AST.AddrSpace as Llvm.AddrSpace
import LLVM.General.AST.Constant as Llvm.Constant
import LLVM.General.AST.Global as Llvm.Global
import LLVM.General.AST.Instruction as Llvm.Instruction
import LLVM.General.AST.IntegerPredicate as Llvm.IntegerPrediacte
import LLVM.General.AST.Name as Llvm.Name
import LLVM.General.AST.Operand as Llvm.Operand
import LLVM.General.AST.RMWOperation as Llvm.RMWOperation
import LLVM.General.AST.Type as Llvm.Type

-- BUILTINS AND RUNTIME --
--------------------------
defAddrSpace :: AddrSpace
defAddrSpace = AddrSpace 0

intType, charType, voidPtrType, stringType :: Llvm.Type.Type
intType = IntegerType 32
charType = IntegerType 8
voidPtrType = PointerType charType defAddrSpace
stringType = PointerType charType defAddrSpace

builtinGlobals :: [Definition]
builtinGlobals = map GlobalDefinition [
      declare "rc_malloc" [intType] voidPtrType
    , declare "rc_retain" [voidPtrType] VoidType
    , declare "rc_release" [voidPtrType] VoidType
    , declare "string_concat" [stringType, stringType] stringType
    , declare "array_malloc" [intType, intType] voidPtrType
    , declare "array_length" [voidPtrType] intType
    , declare "printInt" [intType] VoidType
    , declare "readInt" [] intType
    , declare "printString" [stringType] VoidType
    , declare "readString" [] stringType
    , declare "error" [] VoidType
  ]
  where
    declare str args ret = functionDefaults {
          name = Name str
        , parameters = ([Parameter typ (UnName 0) [] | typ <- args], False)
        , returnType = ret
      }

-- EMITTING CLASS HIERARCHY --
------------------------------
emitHierarchy :: EmitterEnv -> ClassHierarchy -> ErrorInfoT Identity [LlvmModule]
emitHierarchy env hierarchy = do
  layout <- lift $ layoutHierarchy hierarchy
  content <- runEmitterM (env { emitterenvLayout = layout }) $ do
    tell builtinGlobals
    Traversable.mapM emit hierarchy
  let modName = emitterenvModuleName env
  return [LlvmModule defaultModule {
          moduleName = modName
        , moduleDefinitions = content
      }]

-- EMITTER STATE --
-------------------
data EmitterState = EmitterState {
    emitterstateNextId         :: Int
  , emitterstateBlocks         :: Map.Map Name BasicBlock
  , emitterstateBlockStub      :: BasicBlock
  , emitterstatePendingRelease :: [Name]
} deriving (Show)

emitterstate0 :: EmitterState
emitterstate0 = EmitterState 0 Map.empty undefined []

-- EMITTER ENVIRONMENT --
-------------------------
data EmitterEnv = EmitterEnv {
    emitterenvModuleName :: String
  , emitterenvLayout     :: HierarchyLayout
} deriving (Show)

emitterenv0 :: EmitterEnv
emitterenv0 = EmitterEnv "unknown-module" undefined

-- THE MONAD --
---------------
type EmitterM a = WriterT [Definition] (StateT EmitterState (ReaderT EmitterEnv (ErrorInfoT Identity))) a
runEmitterM :: EmitterEnv -> EmitterM a -> ErrorInfoT Identity [Definition]
runEmitterM env action = runReaderT (evalStateT (execWriterT action) emitterstate0) env

-- MONADIC HELPERS --
---------------------
tell1 :: Definition -> EmitterM ()
tell1 = tell . return

nextId :: EmitterM Int
nextId = do
  next <- gets emitterstateNextId
  modify $ \s -> s { emitterstateNextId = next + 1 }
  return next

-- CODE EMITTER HELPERS --
--------------------------


-- FIXME/
-- TREE TRAVERSING --
---------------------
class Emitable a b | a -> b where
  emit :: a -> EmitterM b

instance Emitable Class () where
  emit _ = return ()

instance Emitable TypeMethod String where
  emit = undefined

instance Emitable TypeBasic String where
  emit (TComposed typ) = emit typ
  emit (TPrimitive typ) = emit typ

instance Emitable TypePrimitive String where
  emit = undefined

instance Emitable TypeComposed String where
  emit = undefined

instance Emitable Field () where
  emit = undefined

instance Emitable Method () where
  emit = undefined

instance Emitable Stmt () where
  emit = undefined

instance Emitable RValue TypeBasic where
  emit = undefined


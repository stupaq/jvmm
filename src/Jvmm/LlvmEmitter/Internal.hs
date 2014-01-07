{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Jvmm.LlvmEmitter.Internal where
import Jvmm.LlvmEmitter.Output
import Jvmm.LlvmEmitter.Layout

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Text (pack, replace, unpack)
import qualified Data.Traversable as Traversable

import Jvmm.Builtins
import Jvmm.Errors (ErrorInfoT)
import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output

import qualified LLVM.General.AST as AST

-- EMITTING CLASS HIERARCHY --
------------------------------
emitHierarchy :: EmitterEnv -> ClassHierarchy -> ErrorInfoT Identity [LlvmModule]
emitHierarchy env hierarchy = do
  let layout = runIdentity $ layoutHierarchy hierarchy
  let name = emitterenvModuleName env
  content <- runEmitterM (env { emitterenvLayout = layout }) $ Traversable.mapM emit hierarchy
  return [LlvmModule name content]

-- EMITTER STATE --
-------------------
data EmitterState = EmitterState {
    emitterstateNextInt :: Int
  , emitterstateBlock :: BasicBlockState
} deriving (Show)

emitterstate0 :: EmitterState
emitterstate0 = EmitterState basicblockstate0

data BasicBlockState = BasicBlockState {}

basicblockstate0 = BasicBlockState

-- EMITTER ENVIRONMENT --
-------------------------
data EmitterEnv = EmitterEnv {
    emitterenvModuleName :: String
  , emitterenvLayout :: HierarchyLayout
} deriving (Show)

emitterenv0 :: EmitterEnv
emitterenv0 = EmitterEnv "unknown" undefined

-- THE MONAD --
---------------
type EmitterM a = StateT EmitterState (ReaderT EmitterEnv (ErrorInfoT Identity)) a
runEmitterM :: EmitterEnv -> EmitterM a -> ErrorInfoT Identity a
runEmitterM env action = runReaderT (evalStateT action emitterstate0) env

-- MONADIC HELPERS --
---------------------
nextInt :: EmitterM Int
nextInt = do
  next <- gets emitterenvNextInt
  modify $ \s -> s { emitterenvNextInt = next + 1 }
  return next

-- FIXME/
-- TREE TRAVERSING --
---------------------
class Emitable a b | a -> b where
  emit :: a -> EmitterM b

instance Emitable Class () where
  emit = undefined

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


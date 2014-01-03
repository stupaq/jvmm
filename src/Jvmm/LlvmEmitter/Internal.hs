{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Jvmm.LlvmEmitter.Internal where
import Jvmm.LlvmEmitter.Output

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

-- A class that holds builtins
builtinsClassName :: String
builtinsClassName = "Runtime"
builtinMethodNames :: [String]
builtinMethodNames = ["printInt", "readInt", "error", "printString", "readString"]

-- EMITTING CLASS HIERARCHY --
------------------------------
emitHierarchy :: EmitterEnv -> ClassHierarchy -> ErrorInfoT Identity [LlvmModule]
emitHierarchy env hierarchy = do
  let name = emitterenvModuleName env
  code <- runEmitterM env $ intercept $ Traversable.mapM emit hierarchy
  return [LlvmModule name code]

-- EMITTER STATE --
-------------------
data EmitterState = EmitterState {
    emitterstateUnique   :: Int
} deriving (Show)

emitterstate0 :: EmitterState
emitterstate0 = EmitterState 0

-- EMITTER ENVIRONMENT --
-------------------------
data EmitterEnv = EmitterEnv {
    emitterenvModuleName :: String
} deriving (Show)

emitterenv0 :: EmitterEnv
emitterenv0 = EmitterEnv "unknown"

-- THE MONAD --
---------------
type EmitterM = StateT EmitterState (ReaderT EmitterEnv (WriterT [LlvmLine] (ErrorInfoT Identity)))
runEmitterM :: EmitterEnv -> EmitterM a -> ErrorInfoT Identity a
runEmitterM env action = do
  (res, leftCode) <- runWriterT (runReaderT (evalStateT action emitterstate0) env)
  assert (null leftCode) $ return res

-- MONADIC HELPERS --
---------------------
notImplemented :: a
notImplemented = error "Not implemented"

intercept :: EmitterM a -> EmitterM [LlvmLine]
intercept action = fmap snd $ censor (const []) $ listen action

emitterstateNewId :: EmitterM Int
emitterstateNewId = do
  num <- gets emitterstateUnique
  modify (\st -> st { emitterstateUnique = num + 1 })
  return num

emitterstateResetId :: EmitterM ()
emitterstateResetId = modify (\st -> st { emitterstateUnique = 0 })

-- LABELS ALLOCATION --
-----------------------
newtype Label = Label String
  deriving (Show, Eq, Ord)

newLabel :: EmitterM Label
newLabel = fmap (Label . ('l':) . show)  emitterstateNewId

newLabels2 :: EmitterM (Label, Label)
newLabels2 = liftM2 (,) newLabel newLabel
newLabels3 :: EmitterM (Label, Label, Label)
newLabels3 = liftM3 (,,) newLabel newLabel newLabel
newLabels4 :: EmitterM (Label, Label, Label, Label)
newLabels4 = liftM4 (,,,) newLabel newLabel newLabel newLabel

-- NAMES ALLOCATION --
----------------------
data Name =
    Local String
  deriving (Show, Eq, Ord)

newLocal :: EmitterM Name
newLocal = fmap (Local . ('t':) . show) emitterstateNewId

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
  emit typ = undefined

instance Emitable TypeComposed String where
  emit = undefined

instance Emitable Field () where
  emit = undefined

instance Emitable Method () where
  emit = undefined

instance Emitable Stmt () where
  emit x = undefined

instance Emitable RValue TypeBasic where
  emit x = undefined

-- JUMPING CODE --
------------------
class EmitableConditional a where
  emitCond :: a -> Label -> Label -> EmitterM ()
  evalCond :: a -> EmitterM TypeBasic
  evalCond x = undefined

instance EmitableConditional RValue where
  emitCond x ltrue lfalse = undefined

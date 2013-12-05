{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Jvmm.JvmEmitter.Internal where
import Jvmm.JvmEmitter.Output

import Control.Exception
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Traversable as Traversable

import qualified Jvmm.Errors as Err
import Jvmm.Errors (orThrow, rethrow, ErrorInfoT)
import Jvmm.Builtins
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- EMITTING CLASS HIERARCHY --
------------------------------
emitTopLevelStatics :: String -> ClassHierarchy -> ErrorInfoT Identity [JasminAsm]
emitTopLevelStatics className = execWriterT . Traversable.mapM processClass
  where
    processClass :: Class -> WriterT [JasminAsm] (ErrorInfoT Identity) ()
    processClass clazz@(Class {}) =
      let env = emitterenv0 { emitterenvOverrideClass = Just (ClassName className) }
      in tell . (:[]) =<< lift (runEmitterM  env $ emit clazz)

-- EMITTING STATE --
--------------------
data EmitterState = EmitterState {
    emitterstateStackMax :: Int
} deriving (Show)

emitterstate0 = EmitterState 0

-- EMITTING ENV --
--------------------
data EmitterEnv = EmitterEnv {
    emitterenvStack :: Int
  , emitterenvOverrideClass :: Maybe ClassName
} deriving (Show)

emitterenv0 = EmitterEnv 0 Nothing

-- EMITTER MONAD --
-------------------
type EmitterM = StateT EmitterState (ReaderT EmitterEnv (WriterT [JasminLine] (ErrorInfoT Identity)))
runEmitterM :: EmitterEnv -> EmitterM a -> ErrorInfoT Identity a
runEmitterM env action = do
  (res, log) <- runWriterT (runReaderT (evalStateT action emitterstate0) env)
  assert (log == []) $ return res

notImplemented :: a
notImplemented = error "Not implemented"

intercept :: EmitterM a -> EmitterM [JasminLine]
intercept action =
  censor (const []) $ do
    (_, log) <- listen action
    return log

pushes :: EmitterM a -> EmitterM a
pushes action = do
  stack <- fmap (+1) $ asks emitterenvStack
  local (\env -> env { emitterenvStack = stack }) $ do
    stackMax <- gets emitterstateStackMax
    modify (\st -> st { emitterstateStackMax = maximum [stack, stackMax] })
    action

newStack :: EmitterM ()
newStack = modify (\st -> st { emitterstateStackMax = 0 })

nothing :: EmitterM ()
nothing = return ()

inss :: [String] -> EmitterM ()
inss = tell . map JasminInstruction

ins :: String -> EmitterM ()
ins = inss . return

dir :: String -> EmitterM ()
dir = tell . return . JasminDirective

com :: String -> EmitterM ()
com = tell . return . JasminComment

-- TREE TRAVERSING --
---------------------
class Emitable a b where
  emit :: a -> EmitterM b

instance Emitable Class JasminAsm where
  -- TObject is special, we have to translate it to Java's Object
  emit clazz@(Class TObject super [] [] statics _) = do
    className <- asks emitterenvOverrideClass
    case className of
      Just (ClassName str) -> toJasminClass str $ do
        dir $ ".class public " ++ str
        -- TODO
      Nothing -> notImplemented
    where
      toJasminClass name = fmap (JasminAsm name) . intercept
  emit _ = notImplemented

instance Emitable TypeComposed String where
  emit TObject = return "java/lang/Object"
  emit TString = return "java/lang/String"
  emit _ = notImplemented

instance Emitable Field () where

emitInstance, emitStatic :: Method -> EmitterM ()
emitInstance = undefined
emitStatic = undefined

instance Emitable Method () where

instance Emitable Stmt () where
  emit x = case x of
    SEmpty -> nothing
    SBlock stmts -> mapM_ (emit :: Stmt -> EmitterM ()) stmts
    SExpr expr -> undefined
    -- Memory access
    SStore num expr _ -> undefined
    SStoreArray num expr1 expr2 _ -> undefined
    SPutField num name expr _ -> undefined
    -- Control statements
    SReturn expr _ -> undefined
    SReturnV -> undefined
    SIf expr stmt -> undefined
    SIfElse expr stmt1 stmt2 -> undefined
    SWhile expr stmt -> undefined
    SThrow expr -> undefined
    STryCatch stmt1 typ num stmt2 -> undefined
    -- Special function bodies
    SBuiltin -> undefined
    SInherited -> undefined
    -- Metainformation carriers
    SMetaLocation loc stmts -> undefined
    -- These statements will be replaced with ones caring more context in subsequent phases
    T_SDeclVar _ _ -> Err.unreachable x
    T_SAssign _ _ -> Err.unreachable x
    T_SAssignArr _ _ _ -> Err.unreachable x
    T_SAssignFld _ _ _ -> Err.unreachable x
    T_STryCatch _ _ _ _ -> Err.unreachable x

instance Emitable Expr () where
  emit x = case x of
    -- Literals
    ENull -> undefined
    ELitTrue -> undefined
    ELitFalse -> undefined
    ELitChar _ -> undefined
    ELitString _ -> undefined
    ELitInt n -> undefined
    -- Memory access
    ELoad num _ -> undefined
    EArrayLoad expr1 expr2 _ -> undefined
    EGetField expr name _ -> undefined
    -- Method calls
    EInvokeStatic name exprs -> undefined
    EInvokeVirtual expr name exprs -> undefined
    -- Object creation
    ENewObj typ -> undefined
    ENewArr typ expr -> undefined
    -- Operations
    EUnary op expr _ -> undefined
    EBinary opbin expr1 expr2 _ -> undefined
    -- These expressions will be replaced with ones caring more context in subsequent phases
    T_EVar _ -> Err.unreachable x


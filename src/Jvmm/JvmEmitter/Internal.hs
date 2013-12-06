{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
module Jvmm.JvmEmitter.Internal where
import Jvmm.JvmEmitter.Output

import Control.Exception
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative

import qualified Data.Char as Char
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

-- EMITTING ENVIRONMENT --
--------------------------
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
intercept action = fmap snd $ censor (const []) $ listen action

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

insModifier :: TypeBasic -> Char
insModifier (TComposed _) = 'a'
insModifier (TPrimitive _) = 'i'

insm :: TypeBasic -> String -> EmitterM ()
insm typ = ins . (insModifier typ:)

insmr :: TypeBasic -> String -> EmitterM TypeBasic
insmr typ str = do
  ins (insModifier typ:str)
  return typ

dir :: String -> EmitterM ()
dir = tell . return . JasminDirective

com :: String -> EmitterM ()
com = tell . return . JasminComment

nl :: EmitterM ()
nl = tell $ return JasminEmpty

-- We copy parts of bytecodde verifier functionality here
assertStack :: (Int -> Bool) -> EmitterM a -> EmitterM a
assertStack pred x = do
  stack <- asks emitterenvStack
  assert (pred stack) x

-- EMITTER HELPERS --
---------------------
var :: VariableNum -> String
var = show . fromEnum

-- TREE TRAVERSING --
---------------------
class Emitable a b | a -> b where
  emit :: a -> EmitterM b

instance Emitable Class JasminAsm where
  -- TObject is special, we have to translate it to Java's Object
  emit clazz@(Class TObject super [] [] statics _) = do
    className <- asks emitterenvOverrideClass
    case className of
      Just (ClassName str) -> toJasminClass str $ do
        -- Class that will hold top-level static methods
        dir $ "class public " ++ str
        dir "super java/lang/Object"
        nl
        -- Standart initializer
        dir "method public <init>()V"
        dir "limit locals 0"
        dir "limit stack 1"
        ins "aload_0"
        ins "invokespecial java/lang/Object/<init>()V"
        ins "return"
        dir "end method"
        nl
        -- Static top-level methods
        forM_ statics emit
      Nothing -> notImplemented
    where
      toJasminClass name = fmap (JasminAsm name) . intercept
  emit _ = notImplemented

instance Emitable TypeMethod String where
  emit (TypeMethod tret targs []) = do
    ret <- emit tret
    args <- mapM emit targs
    return $ concat $ concat [["("], args, [")", ret]]
  emit _ = notImplemented

instance Emitable TypeBasic String where
  emit (TComposed typ) = emit typ
  emit (TPrimitive typ) = emit typ

instance Emitable TypePrimitive String where
  emit typ = return $ case typ of
      TVoid -> "V"
      TInt -> "I"
      -- This kind of stinks we won't be able to performa ny better knwoing that
      -- smth is a character or boolean in fact
      TBool -> "I"
      TChar -> "I"

instance Emitable TypeComposed String where
  emit TObject = return "Ljava/lang/Object;"
  emit TString = return "Ljava/lang/String;"
  emit (TArray typ) = ("[" ++) <$> emit typ
  emit TNull = Err.unreachable TNull
  emit _ = notImplemented

instance Emitable Field () where

instance Emitable Method () where
  emit method@Method { methodBody = SBuiltin } = return ()
  emit method@Method { methodBody = SInherited } = return ()
  -- TODO this is for static method
  emit method@Method { methodName = MethodName name, methodType = typ, methodBody = stmt,
      methodArgs = args, methodVariables = vars } = do
    -- The prologue
    case isEntrypoint method of
      True -> dir "method public static main([Ljava/lang/String;)V"
      False -> do
        tdesc <- emit typ
        dir $ "method public static " ++ name ++ tdesc
    let maxVar = maximum $ (0:) $ map (fromEnum . variableNum) $ args ++ vars
    dir $ "limit locals " ++ (show maxVar)
    -- Emit method body but do not write it yet
    newStack
    body <- intercept $ emit stmt
    maxStack <- gets emitterstateStackMax
    -- Emit stack size limit...
    dir $ "limit stack " ++ (show maxStack)
    -- ...and method body
    tell body
    -- The epilogue
    dir "end method"
    nl

instance Emitable Stmt () where
  emit x = case x of
    SEmpty -> nothing
    SBlock stmts -> mapM_ emit stmts
    SExpr expr -> emit expr >> return ()
    -- Memory access
    SStore num expr typ -> do
      emit expr
      insm typ $ "store " ++ (var num)
    SStoreArray num expr1 expr2 telem -> do
      emit expr1
      emit expr2
      insm telem $ "astore " ++ (var num)
    SPutField _ _ _ _ _ -> notImplemented
    -- Control statements
    SReturn expr typ -> do
      emit expr
      insm typ "return"
    SReturnV -> ins "return"
    SIf expr stmt -> undefined
    SIfElse expr stmt1 stmt2 -> undefined
    SWhile expr stmt -> undefined
    SThrow expr -> notImplemented
    STryCatch stmt1 typ num stmt2 -> notImplemented
    -- Special function bodies
    SBuiltin -> Err.unreachable SBuiltin
    SInherited -> Err.unreachable SInherited
    -- Metainformation carriers
    SMetaLocation loc stmts -> stmtMetaLocation' loc (mapM emit stmts)
    -- These statements will be replaced with ones caring more context in subsequent phases
    T_SDeclVar _ _ -> Err.unreachable x
    T_SAssign _ _ -> Err.unreachable x
    T_SAssignArr _ _ _ -> Err.unreachable x
    T_SAssignFld _ _ _ -> Err.unreachable x
    T_STryCatch _ _ _ _ -> Err.unreachable x

instance Emitable Expr TypeBasic where
  emit x = case x of
    -- Literals
    ENull -> insmr (TComposed TObject) "const_null"
    ELitTrue -> emit $ ELitInt 0
    ELitFalse -> emit $ ELitInt 1
    ELitChar c -> emit $ ELitInt $ toInteger $ Char.ord c
    ELitString s -> do
      ins $ "ldc \"" ++ s ++ "\""
      return $ TComposed TString
    ELitInt n -> insmr (TPrimitive TInt) $ "const " ++ (show n)
    -- Memory access
    ELoad num typ -> insmr typ $ "load " ++ (var num)
    EArrayLoad expr1 expr2 telem -> do
      emit expr1
      emit expr2
      insmr telem "aload"
    EGetField _ _ _ _ -> notImplemented
    -- Method calls
    -- FIXME add method type here
    EInvokeStatic typ name exprs -> undefined
    -- FIXME add method type here
    EInvokeVirtual _ _ _ _ -> notImplemented
    -- Object creation
    ENewObj typ -> notImplemented
    ENewArr typ expr -> notImplemented
    -- Operations
    EUnary op expr tret -> do
      typ <- emit expr
      -- FIXME missing
      return tret
    EBinary opbin expr1 expr2 tret -> do
      typ1 <- emit expr1
      typ1 <- emit expr1
      -- FIXME missing
      return tret
    -- These expressions will be replaced with ones caring more context in subsequent phases
    T_EVar _ -> Err.unreachable x


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

-- A class that holds builtins
builtinsClassName = "Runtime" :: String
builtinMethodNames = ["printInt", "readInt", "error", "printString", "readString"]

-- EMITTING CLASS HIERARCHY --
------------------------------
emitTopLevelStatics :: String -> ClassHierarchy -> ErrorInfoT Identity [JasminAsm]
emitTopLevelStatics className = execWriterT . Traversable.mapM processClass
  where
    processClass :: Class -> WriterT [JasminAsm] (ErrorInfoT Identity) ()
    processClass clazz@(Class {}) =
      let env = emitterenv0 { emitterenvOverrideClass = Just (ClassName className) }
      in tell . (:[]) =<< lift (runEmitterM  env $ emit clazz)

-- EMITTER STATE --
-------------------
data EmitterState = EmitterState {
    emitterstateStackCurrent :: Int
  , emitterstateStackMax :: Int
  , emitterstateNextLabel :: Int
} deriving (Show)

emitterstate0 = EmitterState 0 0 0

-- EMITTER ENVIRONMENT --
-------------------------
data EmitterEnv = EmitterEnv {
    emitterenvOverrideClass :: Maybe ClassName
} deriving (Show)

emitterenv0 = EmitterEnv Nothing

-- THE MONAD --
---------------
type EmitterM = StateT EmitterState (ReaderT EmitterEnv (WriterT [JasminLine] (ErrorInfoT Identity)))
runEmitterM :: EmitterEnv -> EmitterM a -> ErrorInfoT Identity a
runEmitterM env action = do
  (res, log) <- runWriterT (runReaderT (evalStateT action emitterstate0) env)
  assert (null log) $ return res

-- MONADIC HELPERS --
---------------------
notImplemented :: a
notImplemented = error "Not implemented"

intercept :: EmitterM a -> EmitterM [JasminLine]
intercept action = fmap snd $ censor (const []) $ listen action

assertStack :: (Int -> Bool) -> EmitterM a -> EmitterM a
assertStack pred x = do
  stack <- gets emitterstateStackCurrent
  assert (pred stack) x

-- LABELS ALLOCATION --
-----------------------
newtype Label = Label String
  deriving (Show, Eq, Ord)

newLabel :: EmitterM Label
newLabel = do
  num <- gets emitterstateNextLabel
  modify (\st -> st { emitterstateNextLabel = num + 1 })
  return $ Label $ "l" ++ show num

newLabels :: Int -> EmitterM [Label]
newLabels 0 = return []
newLabels n = liftM2 (:) newLabel (newLabels $ n - 1)

resetLabels :: EmitterM ()
resetLabels = modify (\st -> st { emitterstateNextLabel = 0 })

-- GENERATION 0: RAW STACK OPERATIONS --
----------------------------------------
alterStack :: (Int -> Int) -> EmitterM ()
alterStack dif = do
  stack <- dif <$> gets emitterstateStackCurrent
  stackMax <- (maximum . (:[stack])) <$> gets emitterstateStackMax
  -- DEBUG remove when in production
  com $ "Stack: " ++ (show stack) ++ " Max: " ++ (show stackMax)
  modify (\st -> st { emitterstateStackCurrent = stack
                    , emitterstateStackMax = stackMax })

newStack :: EmitterM ()
newStack = modify (\st -> st { emitterstateStackMax = 0
                             , emitterstateStackCurrent = 0 })

-- GENERATION 1: RAW MNEMONICS EMITTERS --
------------------------------------------
none :: EmitterM ()
none = return ()

ins :: String -> EmitterM ()
ins str = do
  assert (str == List.dropWhile Char.isSpace str) $ return ()
  tell [JasminInstruction str]

lab :: Label -> EmitterM ()
lab (Label str) = tell [JasminLabel str]

dir :: String -> EmitterM ()
dir = tell . return . JasminDirective

com :: String -> EmitterM ()
com = tell . return . JasminComment

nl :: EmitterM ()
nl = tell $ return JasminEmpty

-- GENERATION 2: SAFE (STACK AWARE) MNEMONICS EMITTERS --
-------------------------------------------------
jmp :: (Int -> Int) -> Label -> EmitterM ()
jmp stdif (Label lab) = ins ("goto" ++ ' ':lab) >> alterStack stdif

cjmp :: String -> (Int -> Int) -> Label -> EmitterM ()
cjmp str stdif (Label lab)
  | List.isInfixOf "if" str = ins (str ++ ' ':lab) >> alterStack stdif
  | otherwise = Err.unreachable "conditional jump must check condition"

inss :: String -> (Int -> Int) -> EmitterM ()
inss str stdif = ins str >> alterStack stdif

inssc :: String -> (Int -> Int) -> Int -> EmitterM ()
inssc str stdif val = inss (str `param` val) stdif

inssv :: String -> (Int -> Int) -> VariableNum -> EmitterM ()
inssv str stdif num = inssc str stdif (fromEnum num)

element, variable :: TypeBasic -> String -> String
element (TPrimitive TChar) = ('c':)
element (TPrimitive TInt) = ('i':)
element (TPrimitive TBool) = ('b':)
element (TComposed _) = ('a':)
variable (TPrimitive _) = ('i':)
variable (TComposed _) = ('a':)

-- This is awesome in my opinion...
param :: String -> Int -> String
-- Garbage in - garbage out
param "iconst" val
  | val == -1 = "iconst_m1"
  | otherwise = "iconst_" ++ show val
param "sipush" val
  | val >= -1 && val <= 5 = param "iconst" val
param "bipush" val
  | val >= -128 && val <= 127 = param "sipush" val
param (t:mnem) val
  | val >= 0 && val <= 3 && mnem `elem` ["load", "store"] = t:mnem ++ '_':show val
param mnem val = mnem ++ ' ':show val

-- PURE HELPERS --
------------------
inc1, dec1, dec3, set0 :: Int -> Int
inc1 = (+) 1
dec1 = decn 1
dec3 = decn 3
set0 = const 0
decn :: Int -> Int -> Int
decn = flip (-)

classPath :: String -> String
classPath = List.dropWhile ('L' ==) . List.dropWhileEnd (';' ==)

-- TREE TRAVERSING --
---------------------
class Emitable a b | a -> b where
  emit :: a -> EmitterM b

instance Emitable Class JasminAsm where
  -- TObject is special, we have to translate it to Java's Object
  emit clazz@(Class TObject super [] statics _) = do
    className <- asks emitterenvOverrideClass
    case className of
      Just (ClassName str) -> toJasminClass str $ do
        -- Class that will hold top-level static methods
        dir $ "class public " ++ str
        dir "super java/lang/Object"
        nl
        -- Standard initializer
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
  -- Static method
  emit method@Method { methodName = MethodName name, methodType = typ, methodBody = stmt,
      methodArgs = args, methodVariables = vars, methodInstance = False } = do
    -- The prologue
    if isEntrypoint method
      then dir "method public static main([Ljava/lang/String;)V"
      else do
        tdesc <- emit typ
        dir $ "method public static " ++ name ++ tdesc
    let maxVar = maximum $ (0:) $ map (fromEnum . variableNum) $ args ++ vars
    dir $ "limit locals " ++ show (maxVar + 1)
    -- Emit method body but do not write it yet
    newStack
    resetLabels
    body <- intercept $ emit stmt
    maxStack <- gets emitterstateStackMax
    -- Emit stack size limit...
    dir $ "limit stack " ++ show maxStack
    -- ...and method body
    tell body
    -- The epilogue
    dir "end method"
    nl

instance Emitable Stmt () where
  emit x = case x of
    SEmpty -> none
    SBlock stmts -> mapM_ emit stmts
    SExpr expr -> emit expr >> none
    -- Memory access
    SStore num expr typ -> do
      emit expr
      inssv (variable typ "store") dec1 num
    SStoreArray num expr1 expr2 telem -> do
      emit $ ELoad num (TComposed $ TArray telem)
      emit expr1
      emit expr2
      inss (element telem "astore") dec3
    SPutField {} -> notImplemented
    -- Control statements
    SReturn expr typ -> do
      emit expr
      inss (variable typ "return") set0
    SReturnV ->
      inss "return" set0
    SIf expr stmt -> do
      emit expr
      lfalse <- newLabel
      cjmp "ifeq" dec1 lfalse
      emit stmt
      lab lfalse
    SIfElse expr stmt1 stmt2 -> do
      emit expr
      [lfalse, lend] <- newLabels 2
      lab lfalse
      cjmp "ifeq" dec1 lfalse
      emit stmt1
      jmp id lend
      emit stmt2
      lab lend
    SWhile expr stmt -> do
      [lbody, lcond] <- newLabels 2
      jmp id lcond
      lab lbody
      emit stmt
      lab lcond
      emit expr
      cjmp "ifne" dec1 lbody
    SThrow expr -> notImplemented
    STryCatch stmt1 typ num stmt2 -> notImplemented
    -- Special function bodies
    SBuiltin -> Err.unreachable SBuiltin
    SInherited -> Err.unreachable SInherited
    -- Metainformation carriers
    SMetaLocation loc stmts -> stmtMetaLocation' loc (mapM emit stmts)
    -- These statements will be replaced with ones caring more context in subsequent phases
    T_SDeclVar {} -> Err.unreachable x
    T_SAssign {} -> Err.unreachable x
    T_SAssignArr {} -> Err.unreachable x
    T_SAssignFld {} -> Err.unreachable x
    T_STryCatch {} -> Err.unreachable x

instance Emitable Expr TypeBasic where
  emit x = case x of
    -- Literals
    ENull -> do
      inss "aconst_null" inc1
      return (TComposed TObject)
    ELitTrue -> do
      inssc "iconst" inc1 1
      return (TPrimitive TInt)
    ELitFalse -> do
      inssc "iconst" inc1 0
      return (TPrimitive TInt)
    ELitChar c -> do
      emit $ ELitInt (toInteger $ Char.ord c)
      return (TPrimitive TInt)
    ELitString s -> do
      inss ("ldc \"" ++ s ++ "\"") inc1
      return (TComposed TString)
    ELitInt n -> do
      inssc "bipush" inc1 (fromInteger n)
      return (TPrimitive TInt)
    -- Memory access
    ELoad num typ -> do
      inssv (variable typ "load") inc1 num
      return typ
    EArrayLoad expr1 expr2 telem -> do
      emit expr1
      emit expr2
      inss (element telem "aload") dec1
      return telem
    EGetField expr TString (FieldName "length") ftyp -> do
      emit expr
      cdesc <- classPath <$> emit (TComposed TString)
      fdesc <- emit ftyp
      inss ("getfield " ++ cdesc ++ "/length " ++ fdesc) id
      return ftyp
    EGetField expr (TArray _) (FieldName "length") ftyp -> do
      emit expr
      inss "arraylength" id
      return ftyp
    EGetField {} -> notImplemented
    -- Method calls
    EInvokeStatic _ (MethodName name) ftyp@(TypeMethod tret _ _) exprs -> do
      mapM_ emit exprs
      className <- asks emitterenvOverrideClass
      case className of
        Just (ClassName str) -> do
          -- Class that holds top-level static methods
          fdesc <- emit ftyp
          let pops = length exprs - (if tret == (TPrimitive TVoid) then 0 else 1)
          -- In case this is actually a builtin
          let str' = if name `elem` builtinMethodNames then builtinsClassName else str
          inss ("invokestatic " ++ str' ++ "/" ++ name ++ fdesc) (decn pops)
          return tret
        Nothing -> notImplemented
    EInvokeVirtual expr TString (MethodName "charAt") ftyp [expr1] -> do
      emit expr
      emit expr1
      cdesc <- classPath <$> emit TString
      fdesc <- emit ftyp
      inss ("invokevirtual " ++ cdesc ++ "/charAt" ++ fdesc) id
      return (TPrimitive TChar)
    EInvokeVirtual {} -> notImplemented
    -- Object creation
    ENewObj typ -> notImplemented
    ENewArr typ@(TComposed _) expr -> do
      emit expr
      tdesc <- emit typ
      inss ("anewarray " ++ tdesc) id
      return typ
    ENewArr typ@(TPrimitive tprim) expr -> do
      emit expr
      let tdesc = case tprim of
            TChar -> "char"
            TBool -> "boolean"
            TInt -> "int"
      inss ("newarray " ++ tdesc) id
      return typ
    -- Operations
    EUnary op expr tret -> do
      typ <- emit expr
      case op of
        OuNot -> do
          -- Alternatively we can: iconst_1, swap, isub.
          -- Java uses the following pattern:
          [ltrue, lend] <- newLabels 2
          cjmp "ifne" dec1 ltrue
          emit ELitTrue
          jmp dec1 lend
          lab ltrue
          emit ELitFalse
          lab lend
          return typ
        OuNeg -> inss "ineg" id >> return typ
    EBinary ObAnd expr1 expr2 tret -> do
      [lfalse, lend] <- newLabels 2
      emit expr1
      cjmp "ifeq" dec1 lfalse
      emit expr2
      cjmp "ifeq" dec1 lfalse
      emit ELitTrue
      jmp id lend
      lab lfalse
      emit ELitFalse
      lab lend
      return tret
    EBinary ObOr expr1 expr2 tret -> do
      [ltrue, lend] <- newLabels 2
      emit expr1
      cjmp "ifne" dec1 ltrue
      emit expr2
      cjmp "ifne" dec1 ltrue
      emit ELitFalse
      jmp id lend
      lab ltrue
      emit ELitTrue
      lab lend
      return tret
    EBinary ObPlus expr1 expr2 tret@(TComposed TString) -> do
      emit expr1
      emit expr2
      fdesc <- emit $ TypeMethod (TComposed TString) [TComposed TString, TComposed TString] []
      inss ("invokestatic " ++ builtinsClassName ++ "/concat"  ++ fdesc) dec1
      return tret
    EBinary opbin expr1 expr2 tret@(TPrimitive tprim) -> do
      emit expr1
      emit expr2
      case opbin of
        ObTimes -> inss "imul" dec1
        ObDiv -> inss "idiv" dec1
        ObMod -> inss "irem" dec1
        ObPlus -> inss "iadd" dec1
        ObMinus -> inss "isub" dec1
        _ -> do
          -- FIXME
          case opbin of
            ObLTH -> return ()
            ObLEQ -> return ()
            ObGTH -> return ()
            ObGEQ -> return ()
            ObEQU -> return ()
            ObNEQ -> return ()
      return tret
    -- These expressions will be replaced with ones caring more context in subsequent phases
    T_EVar {} -> Err.unreachable x


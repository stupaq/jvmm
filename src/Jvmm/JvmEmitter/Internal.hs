{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Jvmm.JvmEmitter.Internal where
import Jvmm.JvmEmitter.Output

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
emitTopLevelStatics :: EmitterEnv -> ClassHierarchy -> ErrorInfoT Identity [JasminAsm]
emitTopLevelStatics env = execWriterT . Traversable.mapM processClass
  where
    processClass :: Class -> WriterT [JasminAsm] (ErrorInfoT Identity) ()
    processClass clazz@(Class {}) = tell . (:[]) =<< lift (runEmitterM  env $ emit clazz)

-- EMITTER STATE --
-------------------
data EmitterState = EmitterState {
    emitterstateStackCurrent :: Int
  , emitterstateStackMax     :: Int
  , emitterstateNextLabel    :: Int
} deriving (Show)

emitterstate0 :: EmitterState
emitterstate0 = EmitterState 0 0 0

-- EMITTER ENVIRONMENT --
-------------------------
data EmitterEnv = EmitterEnv {
    emitterenvOverrideClass :: Maybe ClassName
  , emitterenvDebugStack    :: Bool
} deriving (Show)

emitterenv0 :: EmitterEnv
emitterenv0 = EmitterEnv Nothing False

-- THE MONAD --
---------------
type EmitterM = StateT EmitterState (ReaderT EmitterEnv (WriterT [JasminLine] (ErrorInfoT Identity)))
runEmitterM :: EmitterEnv -> EmitterM a -> ErrorInfoT Identity a
runEmitterM env action = do
  (res, leftCode) <- runWriterT (runReaderT (evalStateT action emitterstate0) env)
  assert (null leftCode) $ return res

-- MONADIC HELPERS --
---------------------
notImplemented :: a
notImplemented = error "Not implemented"

intercept :: EmitterM a -> EmitterM [JasminLine]
intercept action = fmap snd $ censor (const []) $ listen action

assertStack :: (Int -> Bool) -> EmitterM a -> EmitterM a
assertStack p x = do
  stack <- gets emitterstateStackCurrent
  assert (p stack) x

-- LABELS ALLOCATION --
-----------------------
newtype Label = Label String
  deriving (Show, Eq, Ord)

newLabel :: EmitterM Label
newLabel = do
  num <- gets emitterstateNextLabel
  modify (\st -> st { emitterstateNextLabel = num + 1 })
  return $ Label $ 'l':show num

newLabels2 :: EmitterM (Label, Label)
newLabels2 = liftM2 (,) newLabel newLabel
newLabels3 :: EmitterM (Label, Label, Label)
newLabels3 = liftM3 (,,) newLabel newLabel newLabel
newLabels4 :: EmitterM (Label, Label, Label, Label)
newLabels4 = liftM4 (,,,) newLabel newLabel newLabel newLabel

resetLabels :: EmitterM ()
resetLabels = modify (\st -> st { emitterstateNextLabel = 0 })

-- GENERATION 0: RAW STACK OPERATIONS --
----------------------------------------
type StackDiff = Int -> Int

alterStack :: StackDiff -> EmitterM ()
alterStack dif = do
  stack <- dif <$> gets emitterstateStackCurrent
  stackMax <- (maximum . (:[stack])) <$> gets emitterstateStackMax
  debug <- asks emitterenvDebugStack
  when debug $ com $ "stack: " ++ show stack ++ " max: " ++ show stackMax
  modify (\st -> st { emitterstateStackCurrent = stack
                    , emitterstateStackMax = stackMax })

newStack :: EmitterM ()
newStack = modify (\st -> st { emitterstateStackMax = 0
                             , emitterstateStackCurrent = 0 })

getStack :: EmitterM Int
getStack = gets emitterstateStackCurrent

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

com, com' :: String -> EmitterM ()
com = tell . return . JasminComment . ('\n':)
com' = tell . return . JasminComment

nl :: EmitterM ()
nl = tell $ return JasminEmpty

-- GENERATION 2: SAFE (STACK AWARE) MNEMONIC EMITTERS --
--------------------------------------------------------
type Goto = StackDiff -> Label -> EmitterM ()
type ConditionalJump = String -> Goto

jmp :: Goto
jmp stdif (Label lstr) = ins ("goto" ++ ' ':lstr) >> alterStack stdif

jmps :: Label -> Label -> StackDiff -> EmitterM ()
jmps lgoto lnext stdif
  | lnext == lgoto = alterStack stdif
  | otherwise = jmp stdif lgoto

cjmp :: ConditionalJump
cjmp str stdif (Label lstr)
  | "if" `List.isInfixOf` str = inss (str ++ ' ':lstr) stdif
  | otherwise = Err.unreachable "conditional jump must check condition"

cjmps :: Label -> Label -> Label -> String -> StackDiff -> EmitterM ()
cjmps lthen lelse lnext cond stdif
  | lnext == lthen = jfalse
  | lnext == lelse = jtrue
  | otherwise = jtrue >> jmp id lelse
  where
    jtrue = cjmp cond stdif lthen
    jfalse = cjmp (neg cond) stdif lelse
    neg op =
      let rel = map pack ["eq", "ge", "gt", "le", "lt", "ne"]
          negMap = zip rel $ reverse rel
      in head [ x | rule <- negMap, let x = unpack $ uncurry replace rule $ pack op, x /= op ]

inss :: String -> StackDiff -> EmitterM ()
inss str stdif = ins str >> alterStack stdif

inssc :: String -> StackDiff -> Int -> EmitterM ()
inssc str stdif val = inss (str `param` val) stdif

inssv :: String -> StackDiff -> VariableNum -> EmitterM ()
inssv str stdif num = inssc str stdif (fromEnum num)

element, variable :: TypeBasic -> String -> String
element (TPrimitive TChar) = ('c':)
element (TPrimitive TInt) = ('i':)
element (TPrimitive TBool) = ('b':)
element (TPrimitive TVoid) = Err.unreachable "cannot determine opcode prefix for void type"
element (TComposed _) = ('a':)
variable (TPrimitive _) = ('i':)
variable (TComposed _) = ('a':)

comparison :: OpBin -> EmitterM String
comparison op = case op of
  ObLTH -> return "lt"
  ObLEQ -> return "le"
  ObGTH -> return "gt"
  ObGEQ -> return "ge"
  ObEQU -> return "eq"
  ObNEQ -> return "ne"
  _ -> Err.unreachable "no a comparison operator"

-- This is awesome in my opinion...
param :: String -> Int -> String
-- Garbage in, garbage out
param "iconst" val
  | val == -1 = "iconst_m1"
  | otherwise = "iconst_" ++ show val
param "bipush" val
  | val >= -1 && val <= 5 = param "iconst" val
param "sipush" val
  | val >= -128 && val <= 127 = param "bipush" val
param "ldc" val
  | val >= -32768 && val <= 32767 = param "sipush" val
param (t:mnem) val
  | val >= 0 && val <= 3 && mnem `elem` ["load", "store"] = t:mnem ++ '_':show val
param mnem val = mnem ++ ' ':show val

-- PURE HELPERS --
------------------
inc1, dec1, dec2, dec3, set0 :: StackDiff
inc1 = (+) 1
dec1 = decn 1
dec2 = decn 2
dec3 = decn 3
set0 = const 0
decn :: Int -> StackDiff
decn = flip (-)

classPath :: String -> String
classPath = List.dropWhile ('L' ==) . List.dropWhileEnd (';' ==)

-- TREE TRAVERSING --
---------------------
class Emitable a b | a -> b where
  emit :: a -> EmitterM b

instance Emitable Class JasminAsm where
  -- TObject is special, we have to translate it to Java's Object
  emit (Class TObject _ [] statics _) = do
    className <- asks emitterenvOverrideClass
    case className of
      Just (ClassName str) -> toJasminClass str $ do
        -- Class that will hold top-level static methods
        dir $ "class public " ++ str
        dir "super java/lang/Object"
        nl
        -- Standard initializer
        dir "method public <init>()V"
        dir "limit locals 1"
        dir "limit stack 1"
        ins "aload_0"
        ins "invokespecial java/lang/Object/<init>()V"
        ins "return"
        dir "end method"
        nl
        dir "method public static main([Ljava/lang/String;)V"
        dir "limit locals 1"
        dir "limit stack 1"
        tdesc <- emit entrypointType
        let (MethodName name) = entrypointName
        ins $ "invokestatic " ++ str ++ "/" ++ name ++ tdesc
        ins "invokestatic java/lang/System/exit(I)V"
        ins "return"
        dir "end method"
        nl
        -- Static top-level methods
        forM_ statics emit
        nl
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
      TBool -> "Z"
      TChar -> "C"

instance Emitable TypeComposed String where
  emit TObject = return "Ljava/lang/Object;"
  emit TString = return "Ljava/lang/String;"
  emit (TArray typ) = ("[" ++) <$> emit typ
  emit TNull = Err.unreachable TNull
  emit _ = notImplemented

instance Emitable Field () where
  emit = notImplemented

instance Emitable Method () where
  emit Method { methodBody = SBuiltin } = return ()
  emit Method { methodBody = SInherited } = return ()
  -- Static method
  emit Method { methodName = MethodName name, methodType = typ@(TypeMethod tret _ _),
       methodBody = stmt, methodArgs = args, methodVariables = vars, methodInstance = False } = do
    -- The prologue
    tdesc <- emit typ
    dir $ "method public static " ++ name ++ tdesc
    let maxVar = maximum $ (negate 1:) $ map (fromEnum . variableNum) $ args ++ vars
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
    -- ...and final return/nop (just in case there was a branch after return or empty method)
    if tret == TPrimitive TVoid
    then ins "return"
    else unless (not (null body) && isInstruction (last body)) $ ins "nop"
    -- The epilogue
    dir "end method"
    nl
  emit _ = notImplemented

instance Emitable Stmt () where
  emit x = case x of
    SEmpty -> none
    SBlock stmts -> mapM_ emit stmts
    SExpr expr -> do
      emit expr
      n <- getStack
      replicateM_ n $ inss "pop" dec1
    -- Memory access
    -- Order of expressions evaluation is 'left to right'
    SAssign (LVariable num typ) expr -> do
      emit expr
      inssv (variable typ "store") dec1 num
    SAssign (LArrayElement lval expr1 telem) expr2 -> do
      emit $ toRValue lval
      emit expr1
      emit expr2
      inss (element telem "astore") dec3
    SAssign (PruneLExpr _) _ -> Err.unreachable x
    SAssign {} -> notImplemented
    -- Control statements
    SReturn expr typ -> do
      emit expr
      inss (variable typ "return") set0
    SReturnV ->
      inss "return" set0
    SIf expr stmt -> do
      (ltrue, lfalse) <- newLabels2
      emitCond expr ltrue lfalse ltrue
      lab ltrue
      emit stmt
      lab lfalse
    SIfElse expr stmt1 stmt2 -> do
      (ltrue, lfalse, lend) <- newLabels3
      emitCond expr ltrue lfalse ltrue
      lab ltrue
      emit stmt1
      jmp id lend
      lab lfalse
      emit stmt2
      lab lend
    SWhile expr stmt -> do
      (lbody, lcond, lend) <- newLabels3
      jmp id lcond
      lab lbody
      emit stmt
      lab lcond
      emitCond expr lbody lend lend
      lab lend
    SThrow {} -> notImplemented
    STryCatch {} -> notImplemented
    -- Special function bodies
    SBuiltin -> Err.unreachable SBuiltin
    SInherited -> Err.unreachable SInherited
    -- Metainformation carriers
    SMetaLocation loc stmts -> stmtMetaLocation' loc (mapM emit stmts)
    -- These statements will be replaced with ones caring more context in subsequent phases
    PruneSDeclVar {} -> Err.unreachable x
    PruneSTryCatch {} -> Err.unreachable x

instance Emitable RValue TypeBasic where
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
      -- Show will automatically escape things as needed and add quotes
      inss ("ldc " ++ show s) inc1
      return (TComposed TString)
    ELitInt n -> do
      inssc "ldc" inc1 (fromInteger n)
      return (TPrimitive TInt)
    -- Memory access
    -- Order of expressions evaluation is 'left to right'
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
      inss ("invokevirtual " ++ cdesc ++ "/length()" ++ fdesc) id
      return ftyp
    EGetField expr (TArray _) (FieldName "length") ftyp -> do
      emit expr
      inss "arraylength" id
      return ftyp
    EGetField {} -> notImplemented
    -- Method calls
    EInvokeStatic _ (MethodName name) mtyp@(TypeMethod tret _ _) exprs -> do
      mapM_ emit exprs
      className <- asks emitterenvOverrideClass
      case className of
        Just (ClassName str) -> do
          -- Class that holds top-level static methods
          fdesc <- emit mtyp
          let pops = length exprs - (if tret == TPrimitive TVoid then 0 else 1)
          -- In case this is actually a builtin
          let str' = if name `elem` builtinMethodNames then builtinsClassName else str
          inss ("invokestatic " ++ str' ++ "/" ++ name ++ fdesc) (decn pops)
          return tret
        Nothing -> notImplemented
    EInvokeVirtual expr TString (MethodName "charAt") mtyp [expr1] -> do
      emit expr
      emit expr1
      cdesc <- classPath <$> emit TString
      fdesc <- emit mtyp
      inss ("invokevirtual " ++ cdesc ++ "/charAt" ++ fdesc) dec1
      return (TPrimitive TChar)
    EInvokeVirtual {} -> notImplemented
    -- Object creation
    ENewObj _ -> notImplemented
    ENewArr typ@(TComposed tcomp) expr -> do
      emit expr
      tdesc <- case tcomp of
        TArray _ -> emit typ
        _ -> classPath <$> emit typ
      inss ("anewarray " ++ tdesc) id
      return typ
    ENewArr typ@(TPrimitive tprim) expr -> do
      emit expr
      let tdesc = case tprim of
            TChar -> "char"
            TBool -> "boolean"
            TInt -> "int"
            TVoid -> Err.unreachable "void array creation prohibited"
      inss ("newarray " ++ tdesc) id
      return typ
    -- Operations
    EUnary _ _ (TPrimitive TBool) -> evalCond x
    EUnary OuNeg expr tret -> do
      emit expr
      inss "ineg" id
      return tret
    EUnary {} -> Err.unreachable x
    EBinary _ _ _ (TPrimitive TBool) -> evalCond x
    EBinary ObPlus expr1 expr2 tret@(TComposed TString) -> do
      emit expr1
      emit expr2
      fdesc <- emit $ TypeMethod (TComposed TString) [TComposed TString, TComposed TString] []
      inss ("invokestatic " ++ builtinsClassName ++ "/concat"  ++ fdesc) dec1
      return tret
    EBinary opbin expr1 expr2 tret@(TPrimitive _) -> do
      emit expr1
      emit expr2
      case opbin of
        ObTimes -> inss "imul" dec1
        ObDiv -> inss "idiv" dec1
        ObMod -> inss "irem" dec1
        ObPlus -> inss "iadd" dec1
        ObMinus -> inss "isub" dec1
        _ -> Err.unreachable "should be handled in TPrimitive TBool branch"
      return tret
    EBinary {} -> Err.unreachable x
    -- These expressions will be replaced with ones caring more context in subsequent phases
    PruneEVar {} -> Err.unreachable x

-- JUMPING CODE --
------------------
class EmitableConditional a where
  emitCond :: a -> Label -> Label -> Label -> EmitterM ()
  evalCond :: a -> EmitterM TypeBasic
  evalCond x = do
    (ltrue, lfalse, lend) <- newLabels3
    emitCond x ltrue lfalse ltrue
    lab ltrue
    emit ELitTrue
    -- Stack alternation 'dec1' refers to the instruction right after the jump
    jmp dec1 lend
    lab lfalse
    emit ELitFalse
    lab lend
    return (TPrimitive TBool)

instance EmitableConditional RValue where
  emitCond x ltrue lfalse lnext = case x of
    -- Literals
    ELitTrue -> jmp id ltrue
    ELitFalse -> jmp id lfalse
    -- Memory access
    ELoad num typ@(TPrimitive TBool) -> do
      inssv (variable typ "load") inc1 num
      cjmps ltrue lfalse lnext "ifne" dec1
    EArrayLoad expr1 expr2 telem@(TPrimitive TBool) -> do
      emit expr1
      emit expr2
      inss (element telem "aload") dec1
      cjmps ltrue lfalse lnext "ifne" dec1
    EGetField {} -> notImplemented
    -- Method calls
    EInvokeStatic _ (MethodName _) (TypeMethod (TPrimitive TBool) _ _) _ -> do
      emit x
      cjmps ltrue lfalse lnext "ifne" dec1
    EInvokeVirtual {} -> notImplemented
    -- Object creation
    -- Operations
    EUnary OuNot expr _ ->
      emitCond expr lfalse ltrue lnext
    EBinary ObAnd expr1 expr2 _ -> do
      lmid <- newLabel
      emitCond expr1 lmid lfalse lmid
      lab lmid
      emitCond expr2 ltrue lfalse lnext
    EBinary ObOr expr1 expr2 _ -> do
      lmid <- newLabel
      emitCond expr1 ltrue lmid lmid
      lab lmid
      emitCond expr2 ltrue lfalse lnext
    EBinary opbin expr1 expr2 _ -> do
      atyp <- emit expr1
      emit expr2
      comp <- comparison opbin
      cjmps ltrue lfalse lnext ("if_" ++ variable atyp ("cmp" ++ comp)) dec2
    -- If we cannot recognize expression as binary one
    _ -> badExpressionType
    where
      badExpressionType = Err.unreachable $ "attempt to emit jump code for non-boolean" ++ show x


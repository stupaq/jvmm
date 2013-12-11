{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
module Jvmm.Interpreter.Internal where

import qualified System.IO as IO

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Jvmm.Errors (ErrorInfo)
import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output

-- RUNTIME ENVIRONMENT --
-------------------------
data GCConf = GCConf {
    gcthresh :: Int
}

data RunEnv = RunEnv {
    runenvGCConf :: GCConf
  , runenvStatics :: Map.Map TypeComposed Method
  , runenvInstances :: Map.Map TypeComposed Composite
}

buildRunEnv :: ClassHierarchy -> RunEnv
buildRunEnv hierarchy =
  RunEnv {
      runenvGCConf = GCConf 100
    , runenvStatics = statics
    , runenvInstances = instances
  }
  where
    -- FIXME
    instances = Map.empty
    statics = Map.empty

-- RUNTIME STATE --
-------------------
type StackFrame = Map.Map VariableNum PrimitiveValue
stackframe0 :: StackFrame
stackframe0 = Map.empty

mapHead :: (a -> a) -> [a] -> [a]
mapHead fun (x:xs) = (fun x:xs)

type Heap = Map.Map Location ReferencedValue
heap0 :: Heap
heap0 = Map.singleton location0 VNothing

data RunState = RunState {
    runenvStack :: [StackFrame]
  , runenvHeap :: Heap
}

runstate0 :: RunState
runstate0 = RunState {
    runenvStack = [stackframe0]
  , runenvHeap = heap0
}

-- DATA REPRESENTATION --
-------------------------
type Location = Int

location0 :: Location
location0 = 0

data Composite = Composite {
    fields :: Map.Map FieldName PrimitiveValue
} deriving (Show)

composite0 :: Composite
composite0 = Composite {
    fields = Map.empty
}

type Array = Map.Map Int PrimitiveValue

-- Result of executing a statement
data Result =
    -- Value returned from a function
    RValue PrimitiveValue
    -- This is an exception, which can be thrown and caught by a user
  | RException PrimitiveValue
    -- This error can be thrown only by an interpreter itself when irrecoverable
    -- error occurs (null pointer exception, zero division etc.)
  | RError ErrorInfo
  deriving (Show)

instance Error Result where
  strMsg = RError . Err.Bare

data PrimitiveValue =
    -- Things stored on stack are passed by value
    VInt Int
  | VChar Char
  | VBool Bool
  | VRef Location
  | VVoid
  deriving (Eq, Ord, Show)

nullReference :: PrimitiveValue
nullReference = VRef location0

data ReferencedValue =
  -- Things stored on heap are accessible through references
    VArray Array
  | VString String
  | VObject Composite
  | VNothing
  deriving (Show)

defaultValue :: TypeBasic -> InterpreterM PrimitiveValue
defaultValue typ = return $ case typ of
  TPrimitive TInt -> VInt 0
  TPrimitive TChar -> VChar '\0'
  TPrimitive TBool -> VBool False
  TPrimitive TVoid -> VVoid
  TComposed _ -> nullReference

-- THE MONAD --
---------------
-- We need to be able to catch an exception without loosing the state -- this
-- is why we have to reverse the order of ErrorT and StateT
type InterpreterM = ReaderT RunEnv (ErrorT Result (StateT RunState IO))
runInterpreterM :: RunEnv -> InterpreterM a -> IO (Either Result a, RunState)
runInterpreterM r m = runStateT (runErrorT (runReaderT m r)) runstate0

-- MONADIC HELPERS --
---------------------
-- Executes given action in a new 'stack frame', restores old one afterwards.
newFrame :: InterpreterM a -> InterpreterM a
newFrame action = do
  modify (\st -> st { runenvStack = [stackframe0] ++ runenvStack st })
  tryFinally action $
    -- Pop stack frame no matter what happened (exception/error/return),
    -- note that the stack might be modified due to GC runs
    modify (\st -> st { runenvStack = tail $ runenvStack st })
-- When it comes to GC -- note that we cannot really benefit from running gc
-- when exiting from functions (with exception or result). In a good scenario
-- we will immediately pass many stack frames and invoking GC on each one is a
-- total waste of time (we cannot free objects referenced higher in the stack).
-- It is much better to wait for seqential processing statements as returning
-- from a function does not utilize more memory than we were using inside of a
-- function body.

getResult :: InterpreterM () -> InterpreterM PrimitiveValue
getResult action = do
  (action >> throwError (RError Err.nonVoidNoReturn)) `catchError` handler
  where
    handler err = case err of
      RValue val -> return val
      _ -> throwError err

tryCatch :: InterpreterM () -> VariableNum -> InterpreterM () -> InterpreterM ()
tryCatch atry num acatch = do
  atry `catchError` handler
  where
    handler err = case err of
      RException val -> do
        store num val
        acatch
      _ -> throwError err

tryFinally :: InterpreterM a -> InterpreterM () -> InterpreterM a
tryFinally atry afinally = do
  atry `catchError` handler
  where
    handler err = do
      afinally
      throwError err

-- MEMORY MODEL --
------------------
-- If one uses bytecode monadic instruction only, the garbage collection is
-- automatic, these functions should not be invoked directly.
alloc :: ReferencedValue -> InterpreterM PrimitiveValue
alloc val = do
  loc <- gets freeloc
  let ref = VRef loc
  update ref val
  return ref
  where
    freeloc = (+1) . fst . Map.findMax . runenvHeap

deref :: PrimitiveValue -> InterpreterM ReferencedValue
deref (VRef 0) = throwError (RError Err.nullPointerException)
deref (VRef loc) = gets (fromJust . Map.lookup loc . runenvHeap)

update :: PrimitiveValue -> ReferencedValue -> InterpreterM ()
update (VRef loc) val = modify (\st -> st { runenvHeap = Map.insert loc val (runenvHeap st) })

-- GARBAGE COLLECTION --
------------------------
-- Allows running GC in between statements, might silently refuse to make a gc
-- phase if the heap is small enough
runGC :: InterpreterM ()
runGC = do
  heap <- gets runenvHeap
  gcconf <- asks runenvGCConf
  when (doGCNow gcconf heap) $
    modify (\st -> st { runenvHeap = compactHeap (findTopLevelRefs $ runenvStack st) heap })

doGCNow :: GCConf -> Heap -> Bool
doGCNow conf heap = Map.size heap > gcthresh conf

-- Extracts referred locations from stack variables, does not recurse into
-- objects in any way
findTopLevelRefs :: [StackFrame] -> Set.Set Location
findTopLevelRefs = Set.unions . map (Set.fromList . concatMap (\case { VRef loc -> [loc]; _ -> [] }) . Map.elems)

-- Returns garbage-collected heap, each object under location present in
-- provided set is present in the new heap (if it was in the old one)
-- In current implementation locations (addresses) does not change
compactHeap :: Set.Set Location -> Heap -> Heap
compactHeap pinned heap =
  -- Iterate until no change in grey and black sets (<=> grey empty)
  let left = snd $ fun (Set.insert location0 pinned, Set.empty)
  in Map.filterWithKey (\k _ -> Set.member k left) heap
  where
    fun :: (Set.Set Location, Set.Set Location) -> (Set.Set Location, Set.Set Location)
    fun (grey, black)
      | grey == Set.empty = (grey, black)
      | otherwise =
        let (loc, grey') = Set.deleteFindMin grey
            black' = Set.insert loc black
            grey''' = case Map.lookup loc heap of
              Nothing -> error $ Err.danglingReference loc
              Just (VArray arr) -> Map.fold (flip extract) grey' arr
              Just (VObject obj) -> Map.fold (flip extract) grey' (fields obj)
              -- We do not recurse (add to grey) in all other cases
              _ -> grey'
              where
                extract :: Set.Set Location -> PrimitiveValue -> Set.Set Location
                extract grey'' el = case el of
                  VRef loc -> if Set.member loc black' then grey'' else Set.insert loc grey''
                  _ -> grey''
        in fun (grey''', black')

dumpHeap :: InterpreterM ()
dumpHeap = do
  hp <- gets runenvHeap
  liftIO $ IO.hPutStrLn IO.stderr $ "| Heap size: " ++ (show $ Map.size hp)

-- MONADIC BYTECODE --
----------------------
-- One can obtain Jasmin mnemonics by prepending type indicator, and getting
-- arguments from the stack
nop :: InterpreterM ()
nop = return ()

load :: VariableNum -> InterpreterM PrimitiveValue
load num = gets (fromJust . Map.lookup num . head . runenvStack)

aload :: PrimitiveValue -> PrimitiveValue -> InterpreterM PrimitiveValue
aload ref (VInt ind) = do
  VArray arr <- deref ref
  case Map.lookup ind arr of
    Nothing -> throwError (RError $ Err.indexOutOfBounds ind)
    Just val -> return val

store :: VariableNum -> PrimitiveValue -> InterpreterM ()
store num val = do
  modify $ \st -> st { runenvStack = mapHead (Map.insert num val) (runenvStack st) }
  runGC

astore :: PrimitiveValue -> PrimitiveValue -> PrimitiveValue -> InterpreterM ()
astore ref (VInt ind) val = do
  VArray arr <- deref ref
  unless (0 <= ind && ind < Map.size arr) $ throwError (RError $ Err.indexOutOfBounds ind)
  update ref (VArray $ Map.insert ind val arr)
  runGC

invokestatic :: TypeComposed -> MethodName -> [PrimitiveValue] -> InterpreterM ()
invokestatic ctype name args = undefined

throw :: PrimitiveValue -> InterpreterM ()
throw = throwError . RException

return_ :: PrimitiveValue -> InterpreterM ()
return_ = throwError . RValue

return'_ :: InterpreterM ()
return'_ = return_ VVoid

newarray :: TypeBasic -> PrimitiveValue -> InterpreterM PrimitiveValue
newarray typ (VInt len) = do
  val <- defaultValue typ
  alloc $ VArray $ foldl (\m i -> Map.insert i val m) Map.empty [0..(len - 1)]

arraylength :: PrimitiveValue -> InterpreterM PrimitiveValue
arraylength ref = deref ref >>= (\(VArray arr) -> return $ VInt $ Map.size arr)

newobject :: TypeComposed -> InterpreterM PrimitiveValue
newobject typ = alloc $ VObject composite0

getfield :: FieldName -> PrimitiveValue -> InterpreterM PrimitiveValue
-- Types are already checked, we can simply get value from a map
getfield name ref = do
  lval <- deref ref
  case lval of
    VArray _ -> arraylength ref
    VString str -> return $ VInt $ length str
    VObject obj -> return $ fromJust $ Map.lookup name $ fields obj
    _ -> Err.unreachable lval

putfield :: FieldName -> PrimitiveValue -> PrimitiveValue -> InterpreterM ()
-- Types are already checked, we can simply store value in a map
putfield name val ref = do
  lval <- deref ref
  case lval of
    VArray _ -> throwError (RError $ Err.isConstant name)
    VString _ -> throwError (RError $ Err.isConstant name)
    VObject obj -> do
      update ref (VObject $ obj { fields = Map.insert name val (fields obj) })
      runGC
    _ -> Err.unreachable lval

invokevirtual :: MethodName -> PrimitiveValue -> [PrimitiveValue] -> InterpreterM ()
invokevirtual (MethodName "charAt") ref [VInt ind] = do
  VString str <- deref ref
  unless (0 <= ind && ind < length str) $ throwError $ RError $ Err.indexOutOfBounds ind
  return_ $ VChar $ head $ drop ind str
invokevirtual _ _ _ = undefined

-- TREE TRAVERSAL --
--------------------
class Interpretable a b | a -> b where
  interpret :: a -> InterpreterM b

instance Interpretable Stmt () where
  interpret x = case x of
    SEmpty -> nop
    SBlock stmts -> mapM_ interpret stmts
    SExpr expr -> interpret expr >> nop
    -- Memory access
    SStore num expr _ -> interpret expr >>= store num
    SStoreArray num expr1 expr2 _ -> do
      ref <- load num
      ind <- interpret expr1
      val <- interpret expr2
      astore ref ind val
    SPutField num _ name expr _ -> do
      val <- interpret expr
      ref <- load num
      putfield name val ref
    -- Control statements
    SReturn expr _ -> interpret expr >>= return_
    SReturnV -> return'_
    SIf expr stmt -> do
      VBool val <- interpret expr
      if val then interpret stmt else nop
    SIfElse expr stmt1 stmt2 -> do
      VBool val <- interpret expr
      if val then interpret stmt1 else interpret stmt2
    -- Note that (once again) there is no lexical variable hiding, after one
    -- iteration of a loop all variables declared inside fall out of the scope,
    -- which means we can dispose them.
    SWhile expr stmt -> do
      VBool val <- interpret expr
      -- This is a bit hackish, but there is no reason why it won't work and we
      -- want to apply normal chaining rules without too much ifology
      -- Note that GC works fine here because stmt creates heap objects in its own scope
      when val $ interpret $ SBlock [stmt, SWhile expr stmt]
    SThrow expr -> do
      ref <- interpret expr
      throw ref
    STryCatch stmt1 typ num stmt2 -> tryCatch (interpret stmt1) num (interpret stmt2)
    -- Special function bodies
    SBuiltin -> Err.unreachable x
    SInherited -> Err.unreachable x
    -- Metainformation carriers
    SMetaLocation loc stmts ->
      -- We ignore error location for now
      mapM_ interpret stmts
    -- These statements will be replaced with ones caring more context in subsequent phases
    T_SDeclVar {} -> Err.unreachable x
    T_SAssign {} -> Err.unreachable x
    T_SAssignArr {} -> Err.unreachable x
    T_SAssignFld {} -> Err.unreachable x
    T_STryCatch {} -> Err.unreachable x

instance Interpretable Expr PrimitiveValue where
  -- Using JVMM monadic bytecode here might be misleading but during
  -- compilation process we actually turn expression into series of
  -- statements
  interpret x = case x of
    -- Literals
    ENull -> return nullReference
    ELitTrue -> return (VBool True)
    ELitFalse -> return (VBool False)
    ELitChar c -> return (VChar c)
    ELitString str -> alloc (VString str)
    ELitInt n -> return $ VInt $ fromInteger n
    -- Memory access
    ELoad num _ -> load num
    EArrayLoad expr1 expr2 _ -> do
      val1 <- interpret expr1
      val2 <- interpret expr2
      aload val1 val2
    EGetField expr _ num _ -> interpret expr >>= getfield num
    -- Method calls
    EInvokeStatic _ (MethodName "printInt") _ [expr] -> do
      VInt val <- interpret expr
      liftIO $ putStrLn (show val)
      return VVoid
    EInvokeStatic _ (MethodName "readInt") _ [] -> do
      val <- liftIO (readLn :: IO Int)
      return $ VInt val
    EInvokeStatic _ (MethodName "printString") _ [expr] -> do
      ref <- interpret expr
      VString str <- deref ref
      liftIO (putStrLn str)
      return VVoid
    EInvokeStatic _ (MethodName "readString") _ [] -> do
      val <- liftIO (getLine :: IO String)
      ref <- alloc (VString val)
      return ref
    EInvokeStatic _ (MethodName "error") _ [] ->
      throwError $ RError Err.userIssuedError
    EInvokeStatic _ name _ exprs -> mapM interpret exprs >>= (getResult . invokestatic TObject name)
    EInvokeVirtual expr _ name _ exprs -> do
      ref <- interpret expr
      vals <- mapM interpret exprs
      getResult $ invokevirtual name ref vals
    -- Object creation
    ENewObj typ -> newobject typ
    ENewArr typ expr -> interpret expr >>= newarray typ
    -- Operations
    EUnary OuNot expr _ -> do
      VBool val <- interpret expr
      return $ VBool $ not val
    EUnary OuNeg expr _ -> do
      VInt val <- interpret expr
      return $ VInt $ negate val
    EBinary ObPlus expr1 expr2 (TComposed TString) -> do
      VString str1 <- deref =<< interpret expr1
      VString str2 <- deref =<< interpret expr2
      alloc $ VString (str1 ++ str2)
    EBinary opbin expr1 expr2 (TPrimitive TInt) -> do
      VInt val1 <- interpret expr1
      VInt val2 <- interpret expr2
      when (opbin `elem` [ObDiv, ObMod] && val2 == 0) $ throwError $ RError Err.zeroDivision
      return $ VInt $ case opbin of
        ObPlus -> val1 + val2
        ObMinus -> val1 - val2
        ObTimes -> val1 * val2
        ObDiv -> val1 `div` val2
        ObMod -> val1 `rem` val2
        _ -> Err.unreachable opbin
    EBinary ObAnd expr1 expr2 (TPrimitive TBool) -> do
      VBool val1 <- interpret expr1
      case val1 of
        True -> interpret expr2
        False -> return $ VBool False
    EBinary ObOr expr1 expr2 (TPrimitive TBool) -> do
      VBool val1 <- interpret expr1
      case val1 of
        True -> return $ VBool True
        False -> interpret expr2
    EBinary opbin expr1 expr2 (TPrimitive TBool) -> do
      val1 <- interpret expr1
      val2 <- interpret expr2
      return $ VBool $ case opbin of
        -- This works because of 'deriving (Eq, Ord)'
        -- Note that references handling is automatic
        ObEQU -> val1 == val2
        ObNEQ -> val1 /= val2
        ObLTH -> val1 < val2
        ObLEQ -> val1 <= val2
        ObGTH -> val1 > val2
        ObGEQ -> val1 >= val2
        _ -> Err.unreachable opbin
    -- These expressions will be replaced with ones caring more context in subsequent phases
    T_EVar _ -> Err.unreachable x

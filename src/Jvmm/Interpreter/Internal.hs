{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Jvmm.Interpreter.Internal where

import qualified System.IO as IO

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe (fromJust)
import Data.Tree
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable

import Jvmm.Errors (rethrow, ErrorInfo)
import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output

-- RUNTIME ENVIRONMENT --
-------------------------
data GCConf = GCConf {
    gcthresh :: Int
}

type StaticMethods = Map.Map (TypeComposed, MethodName) Method
type InstancePrototypes = Map.Map TypeComposed Composite
data RunEnv = RunEnv {
    runenvGCConf :: GCConf
  , runenvStatics :: StaticMethods
  , runenvInstances :: InstancePrototypes
}

-- BUILDING ENVIRONMENT --
--------------------------
buildRunEnv :: ClassHierarchy -> RunEnv
buildRunEnv hierarchy =
  RunEnv {
      runenvGCConf = GCConf 100
    , runenvStatics = buildStatics hierarchy
    , runenvInstances = buildInstances hierarchy
  }

buildStatics :: ClassHierarchy -> StaticMethods
buildStatics hierarchy = execWriter (Traversable.mapM collect hierarchy)
  where
    collect (Class { classAllMethods = methods, classType = typ@TObject }) =
      forM_ methods $ \x -> case x of
        method@Method { methodInstance = False } -> tell (Map.singleton (typ, methodName method) method)
        _ -> return ()
    -- We do not support static methods defined in classes other than TObject
    collect _ = return ()

buildInstances :: ClassHierarchy -> InstancePrototypes
buildInstances = collect composite0
  where
    collect super (Node clazz@(Class { classType = typ }) children) =
      let composite = collectClass super clazz
      in Map.insert typ composite $ Map.unions $ List.map (collect composite) children
    collectClass super clazz = Composite {
          compositeFields = foldr insertField (compositeFields super) $ classFields clazz
        , compositeMethods = foldr insertMethod (compositeMethods super) $ classAllMethods clazz
        , compositeType = classType clazz
      }
      where
        insertField (Field { fieldType = typ, fieldName = name }) = Map.insert name (defaultValue typ)
        insertMethod (Method { methodBody = SInherited }) = Prelude.id
        insertMethod method@(Method { methodName = name }) = Map.insert name method

-- RUNTIME STATE --
-------------------
type StackFrame = Map.Map VariableNum PrimitiveValue
stackframe0 :: StackFrame
stackframe0 = Map.empty

mapHead :: (a -> a) -> [a] -> [a]
mapHead fun (x:xs) = fun x:xs

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
    compositeFields :: Map.Map FieldName PrimitiveValue
  , compositeMethods :: Map.Map MethodName Method
  , compositeType :: TypeComposed
} deriving (Show)

composite0 :: Composite
composite0 = Composite {
    compositeFields = Map.empty
  , compositeMethods = Map.empty
  , compositeType = undefined
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
    VArray Array TypeBasic
  | VString String
  | VObject Composite
  | VNothing
  deriving (Show)

defaultValue :: TypeBasic -> PrimitiveValue
defaultValue typ = case typ of
  TPrimitive TInt -> VInt 0
  TPrimitive TChar -> VChar '\0'
  TPrimitive TBool -> VBool False
  TPrimitive TVoid -> VVoid
  TComposed _ -> nullReference

runtimeType :: ReferencedValue -> TypeComposed
runtimeType (VString _) = TString
runtimeType (VArray _ typ) = TArray typ
runtimeType (VObject composite) = compositeType composite
runtimeType VNothing = Err.unreachable "nothing has no type"

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
  modify (\st -> st { runenvStack = stackframe0:runenvStack st })
  Err.finally action $
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
getResult action = (action >> return VVoid) `catchError` (\x -> case x of
    RValue val -> return val
    err -> throwError err)

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
-- Allows running GC in between statements, might silently refuse to make a gc phase if the heap is small enough.
-- This can only be called when there is no pending return or exception popping out the stack.
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
findTopLevelRefs = Set.unions . map (Set.fromList . concatMap getLocs . Map.elems)
  where
    getLocs x = case x of
      VRef loc -> [loc]
      _ -> []

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
              Just (VArray arr _) -> Map.fold (flip extract) grey' arr
              Just (VObject obj) -> Map.fold (flip extract) grey' (compositeFields obj)
              -- We do not recurse (add to grey) in all other cases
              _ -> grey'
              where
                extract :: Set.Set Location -> PrimitiveValue -> Set.Set Location
                extract grey'' x = case x of
                  VRef loc' -> if Set.member loc' black' then grey'' else Set.insert loc' grey''
                  _ -> grey''
        in fun (grey''', black')

dumpHeap :: InterpreterM ()
dumpHeap = do
  heap <- gets runenvHeap
  liftIO $ IO.hPutStrLn IO.stderr $ "| Heap size: " ++ show (Map.size heap)

-- MONADIC BYTECODE --
----------------------
-- One can obtain Jasmin mnemonics by prepending type indicator, and getting
-- arguments from the stack
nop :: InterpreterM ()
nop = return ()

load :: VariableNum -> InterpreterM PrimitiveValue
load VariableThis = load (VariableNum 0)
load num = gets (fromJust . Map.lookup num . head . runenvStack)

aload :: PrimitiveValue -> PrimitiveValue -> InterpreterM PrimitiveValue
aload ref (VInt ind) = do
  VArray arr _ <- deref ref
  case Map.lookup ind arr of
    Nothing -> throwError (RError $ Err.indexOutOfBounds ind)
    Just val -> return val

store :: VariableNum -> PrimitiveValue -> InterpreterM ()
store VariableThis val = store (VariableNum 0) val
store num val = do
  modify $ \st -> st { runenvStack = mapHead (Map.insert num val) (runenvStack st) }
  runGC

astore :: PrimitiveValue -> PrimitiveValue -> PrimitiveValue -> InterpreterM ()
astore ref (VInt ind) val = do
  VArray arr typ <- deref ref
  unless (0 <= ind && ind < Map.size arr) $ throwError (RError $ Err.indexOutOfBounds ind)
  update ref $ VArray (Map.insert ind val arr) typ
  runGC

invokestatic :: TypeComposed -> MethodName -> [PrimitiveValue] -> InterpreterM ()
invokestatic TObject (MethodName "printInt") [VInt val] = do
  liftIO $ print val
  return'_
invokestatic TObject (MethodName "readInt") [] = do
  val <- liftIO (readLn :: IO Int)
  return_ $ VInt val
invokestatic TObject (MethodName "printString") [ref] = do
  VString str <- deref ref
  liftIO (putStrLn str)
  return_ VVoid
invokestatic TObject (MethodName "readString") [] = do
  val <- liftIO (getLine :: IO String)
  ref <- alloc (VString val)
  return_ ref
invokestatic TObject (MethodName "error") [] =
  throwError $ RError Err.userIssuedError
invokestatic ctype name args = do
  method <- asks $ fromJust . Map.lookup (ctype, name) . runenvStatics
  newFrame $ do
    zipWithM_ (\num val -> store (VariableNum num) val) [0..] args
    interpret $ methodBody method

throw :: PrimitiveValue -> InterpreterM ()
throw = throwError . RException

return_ :: PrimitiveValue -> InterpreterM ()
return_ = throwError . RValue

return'_ :: InterpreterM ()
return'_ = return_ VVoid

newarray :: TypeBasic -> PrimitiveValue -> InterpreterM PrimitiveValue
newarray typ (VInt len) = do
  let val = defaultValue typ
  alloc $ VArray (foldl (\m i -> Map.insert i val m) Map.empty [0..(len - 1)]) typ

arraylength :: PrimitiveValue -> InterpreterM PrimitiveValue
arraylength ref = deref ref >>= (\(VArray arr _) -> return $ VInt $ Map.size arr)

newobject :: TypeComposed -> InterpreterM PrimitiveValue
newobject ctyp = do
  inst <- asks (fromJust . Map.lookup ctyp . runenvInstances)
  alloc $ VObject inst

getfield :: FieldName -> PrimitiveValue -> InterpreterM PrimitiveValue
-- Types are already checked, we can simply get value from a map
getfield name ref = deref ref >>= \obj -> case obj of
    VArray _ _ -> arraylength ref
    VString str -> return $ VInt $ length str
    VObject composite -> return $ fromJust $ Map.lookup name $ compositeFields composite
    _ -> Err.unreachable obj

putfield :: FieldName -> PrimitiveValue -> PrimitiveValue -> InterpreterM ()
-- Types are already checked, we can simply store value in a map
putfield name@(FieldName y) val ref = deref ref >>= \x -> case (x, y) of
    (VArray _ _, "length") -> throwError (RError $ Err.isConstant name)
    (VString _, "length") -> throwError (RError $ Err.isConstant name)
    (VObject composite, _) -> do
      update ref (VObject $ composite {
          compositeFields = Map.insert name val (compositeFields composite)
        })
      runGC
    _ -> Err.unreachable x

invokevirtual :: PrimitiveValue -> MethodName -> [PrimitiveValue] -> InterpreterM ()
invokevirtual ref name@(MethodName y) args = deref ref >>= \x -> case (x, y) of
    (VString str, "charAt") -> do
      let VInt ind = head args
      guard (0 <= ind && ind < length str) `rethrow` RError (Err.indexOutOfBounds ind)
      return_ $ VChar $ head $ drop ind str
    (VObject composite, _) ->
      let method = fromJust $ Map.lookup name $ compositeMethods composite
      in newFrame $ do
        zipWithM_ (\num val -> store (VariableNum num) val) [0..] (ref:args)
        interpret $ methodBody method
    _ -> Err.unreachable "not a composite"

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
      interpret $ if val then stmt1 else stmt2
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
    STryCatch stmt1 typ num stmt2 -> interpret stmt1 `catchError` \e -> case e of
        RException val -> do
          obj <- deref val
          if runtimeType obj == typ
          then store num val >> interpret stmt2
          else throwError e
        _ -> throwError e
    -- Special function bodies
    SBuiltin -> Err.unreachable x
    SInherited -> Err.unreachable x
    -- Metainformation carriers
    SMetaLocation _ stmts ->
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
    EGetField expr _ name _ -> interpret expr >>= getfield name
    -- Method calls
    EInvokeStatic _ name _ exprs -> do
      vals <- mapM interpret exprs
      getResult $ invokestatic TObject name vals
    EInvokeVirtual expr _ name _ exprs -> do
      ref <- interpret expr
      vals <- mapM interpret exprs
      getResult $ invokevirtual ref name vals
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
      if val1
      then interpret expr2
      else return $ VBool False
    EBinary ObOr expr1 expr2 (TPrimitive TBool) -> do
      VBool val1 <- interpret expr1
      if val1
      then return $ VBool True
      else interpret expr2
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


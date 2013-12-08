module Jvmm.Interpreter (runUnit) where

import qualified System.IO as IO

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Applicative

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Jvmm.Errors (rethrow, ErrorInfoT)
import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output
import qualified Jvmm.Scope as Scope

-- Implements runtime semantics and memory model, note that all type magic
-- should be moved to type cheking phase. Interpreter polimorphism (virtual
-- functions) requires knowledge of runtime type.

-- BUILTINS --
--------------
builtinGlobal = map (\(name, fun) -> (MethodName name, \vals -> fun vals >> nop)) [
  ("printInt",
      \[VInt val] -> do
        liftIO $ putStrLn (show val)
        return'_
  ),
  ("readInt",
      \[] -> do
        val <- liftIO (readLn :: IO Int)
        return_ $ VInt val
  ),
  ("printString",
      \[ref] -> do
        VString str <- deref ref
        liftIO (putStrLn str)
        return'_
  ),
  ("readString",
      \[] -> do
        val <- liftIO (getLine :: IO String)
        ref <- alloc (VString val)
        return_ ref
  ),
  ("error",
      \[] -> throwError $ RError Err.userIssuedError
  )]

-- DATA REPRESENTATION --
-------------------------
type Location = Int
location0 = 0

data Composite = Composite {
  fields :: Map.Map FieldName PrimitiveValue,
  methods :: Map.Map MethodName Stmt
} deriving (Show, Eq, Ord)

composite0 = Composite {
  fields = Map.empty,
  methods = Map.empty
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
  | RError String
  deriving (Eq, Ord, Show)

instance Error Result where
  strMsg s = RError s

data PrimitiveValue =
  -- Things stored on stack are passed by value
    VInt Int
  | VChar Char
  | VBool Bool
  | VRef Location
  | VVoid
  deriving (Eq, Ord, Show)

-- Null reference
ref0 = VRef location0

instance Functor PrimitiveValue where
  fmap f (VInt x) = VInt $ f x
  fmap f (VChar x) = VChar $ f x
  fmap f (VBool x) = VBool $ f x
  fmap f (VRef x) = VRef $ f x
  fmap _ x = x

data ReferencedValue =
  -- Things stored on heap are accessible through references
    VArray Array
  | VString String
  | VObject Composite
  | VNothing
  deriving (Eq, Ord, Show)

defaultValue :: TypeBasic -> InterpreterM PrimitiveValue
defaultValue typ = return $ case typ of
  TPrimitive TInt -> VInt 0
  TPrimitive TChar -> VChar '\0'
  TPrimitive TBool -> VBool False
  TPrimitive TVoid -> VVoid
  TComposed _ -> ref0

-- STATIC ENVIRONMENT --
------------------------
type Func = [PrimitiveValue] -> InterpreterM ()
type FuncEnv = Map.Map MethodName Func
funcenv0 = Map.fromList builtinGlobal

data GCConf = GCConf {
  gcthresh :: Int
}

gcconf0 = GCConf {
  gcthresh = 100
}

data RunEnv = RunEnv {
  funcs :: FuncEnv,
  gcconf :: GCConf
}

runenv0 = RunEnv {
  funcs = funcenv0,
  gcconf = gcconf0
}

-- RUNTIME STATE --
-------------------
type StackFrame = Map.Map VariableNum PrimitiveValue
stackframe0 = Map.empty

mapHead :: (a -> a) -> [a] -> [a]
mapHead fun (x:xs) = (fun x:xs)

type Heap = Map.Map Location ReferencedValue
heap0 = Map.singleton location0 VNothing

data RunState = RunState {
  stack :: [StackFrame],
  heap :: Heap
}

runstate0 = RunState {
  stack = [stackframe0],
  -- Reference with location == 0 is a null reference
  heap = heap0
}

-- THE MONAD --
---------------
-- We need to be able to catch an exception without loosing the state -- this
-- is why we have to reverse the order of ErrorT and StateT
-- Lack of a stack in ReaderT monad (compare with StateT) is caused by the fact
-- that we will never have to inspect hidden part of it
type InterpreterM = ReaderT RunEnv (ErrorT Result (StateT RunState IO))
runInterpreterM :: RunEnv -> RunState -> InterpreterM a -> IO (Either Result a, RunState)
runInterpreterM r s m = runStateT (runErrorT (runReaderT m r)) s

-- Map lookups handler
fromJust :: (Show b) => b -> Maybe a -> a
fromJust ctx Nothing = error (Err.fromJustFailure ctx)
fromJust ctx (Just val) = val

-- Executes given action in a new 'stack frame', restores old one afterwards
-- The assumption that there is no syntactic hiding is EXTREMELY strong and
-- important, we can push new stack frame only when we CALL a function
-- ACHTUNG once again, we do NOT use newFrame outside of a function call!
newFrame :: InterpreterM a -> InterpreterM a
newFrame action = do
  -- Note that all functions have global scope as their base and cannot access
  -- scope of their callers
  modify (\state -> state { stack = [stackframe0] ++ stack state })
  tryFinally action $
    -- Pop stack frame no matter what happened (exception/error/return),
    -- note that the stack might be modified due to GC runs
    modify (\state -> state { stack = tail $ stack state })
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
    handler :: Result -> InterpreterM PrimitiveValue
    handler err = case err of
      RValue val -> return val
      _ -> throwError err

tryCatch :: InterpreterM () -> VariableNum -> InterpreterM () -> InterpreterM ()
tryCatch atry id acatch = do
  atry `catchError` handler
  where
    handler :: Result -> InterpreterM ()
    handler err = case err of
      RException val -> do
        store id val
        acatch
      _ -> throwError err

tryFinally :: InterpreterM a -> InterpreterM () -> InterpreterM a
tryFinally atry afinally = do
  atry `catchError` handler
  where
    handler :: Result -> InterpreterM a
    handler err = do
      afinally
      throwError err

-- MEMORY MODEL --
------------------
-- We should look for references in these places:
-- - stack variables from all frames (be careful with stack variables hiding)
-- - thrown exceptions
-- - returned values
-- - referenced objects (fixpoint)
-- ACHTUNG this requires catching all returns and exceptions (to obtain
-- references) and rethrowing them after GC phase with appropriately changed
-- references
-- ACHTUNG at the moment we only know current stack frame, there might be some
-- references hidden in lower stack frames

-- Allows running GC in between statements, might silently refuse to make a gc
-- phase if the heap is small enough
runGC :: InterpreterM ()
runGC = do
  hp <- gets heap
  gcc <- asks gcconf
  when (doGCNow gcc hp) $ do
    st <- gets stack
    modify (\state -> state { heap = compactHeap (findRefs st) hp })

doGCNow :: GCConf -> Heap -> Bool
doGCNow gcconf heap = Map.size heap > gcthresh gcconf

-- Extracts referred locations from stack variables, does not recurse into
-- objects in any way
findRefs :: [StackFrame] -> Set.Set Location
findRefs = Set.unions . map (Set.fromList . map mp . filter flt . Map.elems)
  where
    flt :: PrimitiveValue -> Bool
    flt (VRef _) = True
    flt _ = False
    mp :: PrimitiveValue -> Location
    mp (VRef loc) = loc

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
                extract grey'' elem = case elem of
                  VRef loc -> case Set.member loc black' of
                    -- Do not search again if already searched
                    True -> grey''
                    False -> Set.insert loc grey''
                  _ -> grey''
        in fun (grey''', black')

-- If one uses bytecode monadic instruction only, the garbage collection is
-- automatic, these functions should not be invoked directly.
alloc :: ReferencedValue -> InterpreterM PrimitiveValue
alloc val = do
  loc <- gets freeloc
  let ref = VRef loc
  update ref val
  return ref
  where
    freeloc :: RunState -> Location
    freeloc = (+1) . fst . Map.findMax . heap -- TODO this is imperfect since we have GC

deref :: PrimitiveValue -> InterpreterM ReferencedValue
deref (VRef 0) = throwError (RError Err.nullPointerException)
deref (VRef loc) = gets (fromJust (VRef loc) . Map.lookup loc . heap)

update :: PrimitiveValue -> ReferencedValue -> InterpreterM ()
update (VRef loc) val = modify (\state -> state { heap = Map.insert loc val (heap state) })

dumpHeap :: InterpreterM ()
dumpHeap = do
  hp <- gets heap
  liftIO $ do
    println $ "| Heap size: " ++ (show $ Map.size hp)

-- MONADIC BYTECODE --
----------------------
-- One can obtain Jasmin mnemonics by prepending type indicator, and getting
-- arguments from the stack
nop :: InterpreterM ()
nop = return ()

load :: VariableNum -> InterpreterM PrimitiveValue
load id = gets (fromJust id . Map.lookup id . head . stack)

aload :: PrimitiveValue -> PrimitiveValue -> InterpreterM PrimitiveValue
aload ref (VInt ind) = do
  VArray arr <- deref ref
  case Map.lookup ind arr of
    Nothing -> throwError (RError $ Err.indexOutOfBounds ind)
    Just val -> return val

store :: VariableNum -> PrimitiveValue -> InterpreterM ()
store id val = do
  modify $ \state -> state { stack = mapHead (Map.insert id val) (stack state) }
  runGC --TODO move to some more sensible place

astore :: PrimitiveValue -> PrimitiveValue -> PrimitiveValue -> InterpreterM ()
astore ref (VInt ind) val = do
  VArray arr <- deref ref
  unless (0 <= ind && ind < Map.size arr) $ throwError (RError $ Err.indexOutOfBounds ind)
  update ref (VArray $ Map.insert ind val arr)
  runGC --TODO move to some more sensible place

invokestatic :: MethodName -> [PrimitiveValue] -> InterpreterM ()
invokestatic id vals = asks (fromJust id . Map.lookup id . funcs) >>= ($ vals)

throw :: PrimitiveValue -> InterpreterM ()
throw = throwError . RException

return_ :: PrimitiveValue -> InterpreterM ()
return_ = throwError . RValue

return'_ :: InterpreterM ()
return'_ = return_ VVoid

newarray :: Type -> PrimitiveValue -> InterpreterM PrimitiveValue
newarray typ (VInt len) = do
  val <- defaultValue typ
  alloc $ VArray $ foldl (\m i -> Map.insert i val m) Map.empty [0..(len - 1)]

arraylength :: PrimitiveValue -> InterpreterM PrimitiveValue
arraylength ref = deref ref >>= (\(VArray arr) -> return $ VInt $ Map.size arr)

newobject :: Type -> InterpreterM PrimitiveValue
newobject typ = alloc $ VObject composite0

getfield :: FieldName -> PrimitiveValue -> InterpreterM PrimitiveValue
-- Types are already checked, we can simply get value from a map
getfield id ref = do
  lval <- deref ref
  case lval of
    VArray _ -> arraylength ref
    VString str -> return $ VInt (length str)
    VObject obj -> case Map.lookup id (fields obj) of
      Just val -> return val
      Nothing -> do
        --FIXME need type information, ps. findWithDefault
        throwError (RError $ "usage of uninitialized class field: " ++ (show id))
    _ -> error $ Err.unreachable lval

putfield :: FieldName -> PrimitiveValue -> PrimitiveValue -> InterpreterM ()
-- Types are already checked, we can simply store value in a map
putfield id val ref = do
  lval <- deref ref
  case lval of
    VArray _ -> throwError (RError $ Err.isConstant id)
    VString _ -> throwError (RError $ Err.isConstant id)
    VObject obj -> do
      update ref (VObject $ obj { fields = Map.insert id val (fields obj) })
      runGC --TODO move to some more sensible place
    _ -> error $ Err.unreachable lval

invokevirtual :: MethodName -> PrimitiveValue -> [PrimitiveValue] -> InterpreterM ()
invokevirtual (MethodName "charAt$0") ref [VInt ind] = do
  VString str <- deref ref
  unless (0 <= ind && ind < length str) $ throwError (RError $ Err.indexOutOfBounds ind)
  return_ $ VChar (head $ drop ind str)
invokevirtual id _ _ = error $ Err.unreachable id

-- TREE TRAVERSAL --
--------------------
println :: String -> IO ()
println = IO.hPutStrLn IO.stderr

runUnit :: Bool -> Stmt -> IO Int
runUnit debug stmt = do
  (x, state) <- runtime stmt
  case x of
    Left (RError err) -> ioError $ Err.userIssuedError err
    Left (RException err) -> ioError $ Err.userIssuedError (Err.uncaughtTopLevel err)
    Left (RValue res) -> do
      when debug $ do
        println $ "+----------------------------------"
        println $ "| Main returned:\t" ++ (show res)
        println $ "| Heap stats:\n|\tallocated_count:\t" ++ (show $ Map.size (heap state Map.\\ heap runstate0))
        println $ "+----------------------------------"
      -- Main function shoul have integer return value
      let VInt ret = res
      return ret
    Right _ -> error $ Err.unreachable x

-- Executes _int main()_ function in given translation unit
runtime :: Stmt -> IO ((Either Result (), RunState))
runtime = runInterpreterM runenv0 runstate0 . interp

-- Declarations
funD :: Stmt -> InterpreterM a -> InterpreterM a
funD (SDefFunc typ id args excepts stmt) =
  -- Functions are defined in global scope, we first scan this scope, collect
  -- definitions and then, using environment with all global scope symbols, we
  -- run our program. As a side effect we obtain mutually recursive functions.
  -- NOTE there is no explicit fixpoint
  local (\env -> env { funcs = Map.insert id fun (funcs env) })
  where
    fun :: Func
    fun vals = newFrame $ do
      -- We will once again use temp variables, this time we do not rewrite code
      -- Note that temporary variables wre present in newFrame, they will
      -- automatically vanish on function exit
      zipWithM_ (\(SDeclVar _ id) var -> store (argId id) var) args vals
      -- This is a hack, but it's extremenly refined one, we get garbage
      -- collection and all shit connected with that for free, it's exactly the
      -- same code as in interp (SLocal ...)
      interp $ SLocal args ((map (\(SDeclVar _ id) -> SAssign id (EVar $ argId id)) args) ++ [stmt])
      -- If function has _void_ return type (but only then) and does not return
      -- explicitly we do it here
      defval <- defaultValue typ
      when (typ == TVoid) (return_ defval)
      where
        argId id = Scope.tempIdent id "arg"
funD (SDeclVar typ id) = (>>) (defaultValue typ >>= store id)
-- TODO declare all member functions when dealing with real classes
funD (SDefClass id super (SGlobal stmts)) = Prelude.id
funD x = error $ Err.unreachable x

-- /FIXME

class Interpretable a b | a -> b where
  interp :: a -> InterpreterM b

instance Interpretable Stmt () where
  interp x = case x of
    SEmpty -> nop
    SBlock stmts -> mapM_ interp stmts
    SExpr expr -> interp expr >> nop
    -- Memory access
    SStore num expr _ -> interp expr >>= store num
    SStoreArray num expr1 expr2 _ -> do
      ref <- load num
      ind <- interp expr1
      val <- interp expr2
      astore ref ind val
    SPutField num _ name _ expr _ -> do
      val <- interp expr
      ref <- load num
      putfield name val ref
    -- Control statements
    SReturn expr _ -> interp expr >>= return_
    SReturnV -> return'_
    SIf expr stmt -> do
      VBool val <- interp expr
      if val then interp stmt else nop
    SIfElse expr stmt1 stmt2 -> do
      VBool val <- interp expr
      if val then interp stmt1 else interp stmt2
    -- Note that (once again) there is no lexical variable hiding, after one
    -- iteration of a loop all variables declared inside fall out of the scope,
    -- which means we can dispose them.
    SWhile expr stmt -> do
      VBool val <- interp expr
      -- This is a bit hackish, but there is no reason why it won't work and we
      -- want to apply normal chaining rules without too much ifology
      -- Note that GC works fine here because stmt creates heap objects in its own scope
      when val $ interp $ SBlock [stmt, SWhile expr stmt]
    SThrow expr -> do
      ref <- interp expr
      throw ref
    STryCatch stmt1 typ num stmt2 -> tryCatch (interp stmt1) num (interp stmt2)
    -- Special function bodies
    SBuiltin -> Err.unreachable x
    SInherited -> undefined
    -- Metainformation carriers
    SMetaLocation loc stmts -> stmtMetaLocation loc $ mapM interp stmts
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
  interp x = case x of
    -- Literals
    ENull -> return ref0
    ELitTrue -> return (VBool True)
    ELitFalse -> return (VBool False)
    ELitChar c -> return (VChar c)
    ELitString str -> alloc (VString str)
    ELitInt n -> VInt <$> fromInteger n
    -- Memory access
    ELoad num _ -> load num
    EArrayLoad expr1 expr2 -> aload <$> interp expr1 <*> interp expr2
    EGetField expr _ num _ -> interp expr >>= getfield num
    -- Method calls
    EInvokeStatic _ name _ exprs -> mapM interp exprs >>= (getResult . invokestatic name)
    EInvokeVirtual expr _ name _ exprs -> do
      ref <- interp expr
      vals <- mapM interp exprs
      getResult $ invokevirtual name ref vals
    -- Object creation
    ENewObj typ -> newobject typ
    ENewArr typ expr -> interp expr >>= newarray typ
    -- Operations
    EUnary OuNot expr _ -> not <$> interp expr
    EUnary OuNeg expr _ -> negate <$> interp expr
    EBinary ObPlus expr1 expr2 TString -> do
      VString str1 <- deref <$> interp expr1
      VString str2 <- deref <$> interp expr2
      alloc $ VString (str1 ++ str2)
    EBinary opbin expr1 expr2 TInt -> do
      VInt val1 <- interp expr1
      VInt val2 <- interp expr2
      when (opbin `elem` [ObDiv, ObMod] && val2 == 0) $ throwError $ RError Err.zeroDivision
      return $ VInt $ case opbin of
        ObPlus -> val1 + val2
        ObMinus -> val1 - val2
        ObTimes -> val1 * val2
        ObDiv -> val1 `div` val2
        ObMod -> val1 `rem` val2 -- I'mma hipst'a!
        _ -> error $ Err.unreachable opbin
    EBinary ObAnd expr1 expr2 TBool -> do
      VBool val1 <- interp expr1
      case val1 of
        True -> interp expr2
        False -> return $ VBool False
    EBinary ObOr expr1 expr2 TBool -> do
      VBool val1 <- interp expr1
      case val1 of
        True -> return $ VBool True
        False -> interp expr2
    EBinary opbin expr1 expr2 TBool -> do
      val1 <- interp expr1
      val2 <- interp expr2
      return $ VBool $ case opbin of
        -- This works because of 'deriving (Eq, Ord)'
        -- Note that references handling is automatic
        ObEQU -> val1 == val2
        ObNEQ -> val1 /= val2
        ObLTH -> val1 < val2
        ObLEQ -> val1 <= val2
        ObGTH -> val1 > val2
        ObGEQ -> val1 >= val2
        _ -> error $ Err.unreachable opbin
    -- These expressions will be replaced with ones caring more context in subsequent phases
    T_EVar _ -> Err.unreachable x


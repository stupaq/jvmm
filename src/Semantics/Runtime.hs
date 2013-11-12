module Semantics.Runtime (runUnit) where

import qualified System.IO as IO

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Semantics.Commons
import Semantics.Errors (rethrow, ErrorInfoT)
import qualified Semantics.Errors as Err
import Semantics.APTree
import qualified Jvmm.Scope as Scope

-- Implements runtime semantics and memory model, note that all type magic
-- should be moved to type cheking phase. Runtime polimorphism (virtual
-- functions) requires knowledge of runtime type.

-- BUILTINS --
--------------
entrypoint = Scope.tagGlobal $ FIdent "main"

builtinGlobal = map (\(name, fun) -> (Scope.tagGlobal $ FIdent name, \vals -> fun vals >> nop)) [
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
      \[] -> throwError $ RError Err.userError
  )]

defaultValue :: Type -> RuntimeM PrimValue
defaultValue typ = return $ case typ of
  TInt -> VInt 0
  TChar -> VChar '\0'
  TBool -> VBool False
  TString -> ref0
  TObject -> ref0
  TArray _ -> ref0
  TUser _ -> ref0
  TVoid -> VVoid

-- LIMITS --
------------
checkVal :: PrimValue -> RuntimeM ()
checkVal val = case val of
  VInt num -> unless (-2147483648 <= num && num <= 2147483647) $ throwError $ RError Err.intOverflow
  _ -> return ()

-- DATA REPRESENTATION --
-------------------------
type Loc = Integer
-- Null location
loc0 = 0

data Composite = Composite {
  fields :: Map.Map UIdent PrimValue,
  methods :: Map.Map UIdent Stmt
} deriving (Eq, Ord, Show)
composite0 = Composite {
  fields = Map.empty,
  methods = Map.empty
}

type Array = Map.Map Int PrimValue

-- Result of executing a statement
data Result =
  -- Value returned from a function
    RValue PrimValue
  -- This is an exception, which can be thrown and caught by a user
  | RException PrimValue
  -- This error can be thrown only by an interpreter itself when irrecoverable
  -- error occurs (null pointer exception, zero division etc.)
  | RError String
  deriving (Eq, Ord, Show)

instance Error Result where
  strMsg s = RError s

data PrimValue =
  -- Things stored on stack and passed by value
    VInt Int
  | VChar Char
  | VBool Bool
  | VRef Loc
  | VVoid
  deriving (Eq, Ord, Show)
-- Null reference
ref0 = VRef loc0

data RefValue =
  -- Things stored on heap are only referenced
    VArray Array
  | VString String
  | VObject Composite
  | VNothing
  deriving (Eq, Ord, Show)

-- STATIC ENVIRONMENT --
------------------------
type Func = [PrimValue] -> RuntimeM ()
type FuncEnv = Map.Map UIdent Func
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
type StackFrame = Map.Map UIdent PrimValue
stackframe0 = Map.empty

mapHead :: (a -> a) -> [a] -> [a]
mapHead fun (x:xs) = (fun x:xs)

type Heap = Map.Map Loc RefValue
heap0 = Map.singleton loc0 VNothing

data RunState = RunState {
  stack :: [StackFrame],
  heap :: Heap
}
runstate0 = RunState {
  stack = [stackframe0],
  -- Reference with location == 0 is a null reference
  heap = heap0
}

-- RUNTIME DEBUG --
-------------------

dumpHeap :: RuntimeM ()
dumpHeap = do
  hp <- gets heap
  liftIO $ do
    println $ "| Heap size: " ++ (show $ Map.size hp)

-- RUNTIME MONAD --
-------------------
-- We need to be able to catch an exception without loosing the state -- this
-- is why we have to reverse the order of ErrorT and StateT
-- Lack of a stack in ReaderT monad (compare with StateT) is caused by the fact
-- that we will never have to inspect hidden part of it
type RuntimeM = ReaderT RunEnv (ErrorT Result (StateT RunState IO))
runRuntimeM :: RunEnv -> RunState -> RuntimeM a -> IO (Either Result a, RunState)
runRuntimeM r s m = runStateT (runErrorT (runReaderT m r)) s

-- Map lookups handler
fromJust :: (Show b) => b -> Maybe a -> a
fromJust ctx Nothing = error (Err.fromJustFailure ctx)
fromJust ctx (Just val) = val

-- Executes given action in a new 'stack frame', restores old one afterwards
-- The assumption that there is no syntactic hiding is EXTREMELY strong and
-- important, we can push new stack frame only when we CALL a function
-- ACHTUNG once again, we do NOT use newFrame outside of a function call!
newFrame :: RuntimeM a -> RuntimeM a
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

getResult :: RuntimeM () -> RuntimeM PrimValue
getResult action = do
  (action >> throwError (RError Err.nonVoidNoReturn)) `catchError` handler
  where
    handler :: Result -> RuntimeM PrimValue
    handler err = case err of
      RValue val -> return val
      _ -> throwError err

tryCatch :: RuntimeM () -> UIdent -> RuntimeM () -> RuntimeM ()
tryCatch atry id acatch = do
  atry `catchError` handler
  where
    handler :: Result -> RuntimeM ()
    handler err = case err of
      RException val -> do
        store id val
        acatch
      _ -> throwError err

tryFinally :: RuntimeM a -> RuntimeM () -> RuntimeM a
tryFinally atry afinally = do
  atry `catchError` handler
  where
    handler :: Result -> RuntimeM a
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
runGC :: RuntimeM ()
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
findRefs :: [StackFrame] -> Set.Set Loc
findRefs = Set.unions . map (Set.fromList . map mp . filter flt . Map.elems)
  where
    flt :: PrimValue -> Bool
    flt (VRef _) = True
    flt _ = False
    mp :: PrimValue -> Loc
    mp (VRef loc) = loc

-- Returns garbage-collected heap, each object under location present in
-- provided set is present in the new heap (if it was in the old one)
-- In current implementation locations (addresses) does not change
compactHeap :: Set.Set Loc -> Heap -> Heap
compactHeap pinned heap =
  -- Iterate until no change in grey and black sets (<=> grey empty)
  let left = snd $ fun (Set.insert loc0 pinned, Set.empty)
  in Map.filterWithKey (\k _ -> Set.member k left) heap
  where
    fun :: (Set.Set Loc, Set.Set Loc) -> (Set.Set Loc, Set.Set Loc)
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
                extract :: Set.Set Loc -> PrimValue -> Set.Set Loc
                extract grey'' elem = case elem of
                  VRef loc -> case Set.member loc black' of
                    -- Do not search again if already searched
                    True -> grey''
                    False -> Set.insert loc grey''
                  _ -> grey''
        in fun (grey''', black')

-- If one uses bytecode monadic instruction only, the garbage collection is
-- automatic, these functions should not be invoked directly.
alloc :: RefValue -> RuntimeM PrimValue
alloc val = do
  loc <- gets freeloc
  let ref = VRef loc
  update ref val
  return ref
  where
    freeloc :: RunState -> Loc
    freeloc = (+1) . fst . Map.findMax . heap -- TODO this is imperfect since we have GC

deref :: PrimValue -> RuntimeM RefValue
deref (VRef 0) = throwError (RError Err.nullPointerException)
deref (VRef loc) = gets (fromJust (VRef loc) . Map.lookup loc . heap)

update :: PrimValue -> RefValue -> RuntimeM ()
update (VRef loc) val = modify (\state -> state { heap = Map.insert loc val (heap state) })

-- JVMM bytecode --
-------------------
-- One can obtain Jasmin mnemonics by prepending type indicator, and getting
-- arguments from the stack
nop :: RuntimeM ()
nop = return ()

load :: UIdent -> RuntimeM PrimValue
load id = gets (fromJust id . Map.lookup id . head . stack)

aload :: PrimValue -> PrimValue -> RuntimeM PrimValue
aload ref (VInt ind) = do
  VArray arr <- deref ref
  case Map.lookup ind arr of
    Nothing -> throwError (RError $ Err.indexOutOfBounds ind)
    Just val -> return val

store :: UIdent -> PrimValue -> RuntimeM ()
store id val = do
  modify $ \state -> state { stack = mapHead (Map.insert id val) (stack state) }
  runGC --TODO move to some more sensible place

astore :: PrimValue -> PrimValue -> PrimValue -> RuntimeM ()
astore ref (VInt ind) val = do
  VArray arr <- deref ref
  unless (0 <= ind && ind < Map.size arr) $ throwError (RError $ Err.indexOutOfBounds ind)
  update ref (VArray $ Map.insert ind val arr)
  runGC --TODO move to some more sensible place

invokestatic :: UIdent -> [PrimValue] -> RuntimeM ()
invokestatic id vals = asks (fromJust id . Map.lookup id . funcs) >>= ($ vals)

throw :: PrimValue -> RuntimeM ()
throw = throwError . RException

return_ :: PrimValue -> RuntimeM ()
return_ = throwError . RValue

return'_ :: RuntimeM ()
return'_ = return_ VVoid

newarray :: Type -> PrimValue -> RuntimeM PrimValue
newarray typ (VInt len) = do
  val <- defaultValue typ
  alloc $ VArray $ foldl (\m i -> Map.insert i val m) Map.empty [0..(len - 1)]

arraylength :: PrimValue -> RuntimeM PrimValue
arraylength ref = deref ref >>= (\(VArray arr) -> return $ VInt $ Map.size arr)

newobject :: Type -> RuntimeM PrimValue
newobject typ = alloc $ VObject composite0

getfield :: UIdent -> PrimValue -> RuntimeM PrimValue
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
    _ -> error $ Err.unusedBranch lval

putfield :: UIdent -> PrimValue -> PrimValue -> RuntimeM ()
-- Types are already checked, we can simply store value in a map
putfield id val ref = do
  lval <- deref ref
  case lval of
    VArray _ -> throwError (RError $ Err.isConstant id)
    VString _ -> throwError (RError $ Err.isConstant id)
    VObject obj -> do
      update ref (VObject $ obj { fields = Map.insert id val (fields obj) })
      runGC --TODO move to some more sensible place
    _ -> error $ Err.unusedBranch lval

invokevirtual :: UIdent -> PrimValue -> [PrimValue] -> RuntimeM ()
invokevirtual (FIdent "charAt$0") ref [VInt ind] = do
  VString str <- deref ref
  unless (0 <= ind && ind < length str) $ throwError (RError $ Err.indexOutOfBounds ind)
  return_ $ VChar (head $ drop ind str)
invokevirtual id _ _ = error $ Err.unusedBranch id

-- DENOTATIONAL SEMANTICS --
----------------------------
println :: String -> IO ()
println = IO.hPutStrLn IO.stderr

runUnit :: Bool -> Stmt -> IO Int
runUnit debug stmt = do
  (x, state) <- runtime stmt
  case x of
    Left (RError err) -> ioError $ userError err
    Left (RException err) -> ioError $ userError (Err.uncaughtTopLevel err)
    Left (RValue res) -> do
      when debug $ do
        println $ "+----------------------------------"
        println $ "| Main returned:\t" ++ (show res)
        println $ "| Heap stats:\n|\tallocated_count:\t" ++ (show $ Map.size (heap state Map.\\ heap runstate0))
        println $ "+----------------------------------"
      -- Main function shoul have integer return value
      let VInt ret = res
      return ret
    Right _ -> error $ Err.unusedBranch x

-- Executes _int main()_ function in given translation unit
runtime :: Stmt -> IO ((Either Result (), RunState))
runtime = runRuntimeM runenv0 runstate0 . funS

-- Declarations
funD :: Stmt -> RuntimeM a -> RuntimeM a
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
      -- same code as in funS (SLocal ...)
      funS $ SLocal args ((map (\(SDeclVar _ id) -> SAssign id (EVar $ argId id)) args) ++ [stmt])
      -- If function has _void_ return type (but only then) and does not return
      -- explicitly we do it here
      defval <- defaultValue typ
      when (typ == TVoid) (return_ defval)
      where
        argId id = Scope.tempIdent id "arg"
funD (SDeclVar typ id) = (>>) (defaultValue typ >>= store id)
-- TODO declare all member functions when dealing with real classes
funD (SDefClass id super (SGlobal stmts)) = Prelude.id
funD x = error $ Err.unusedBranch x

-- Statements
funS :: Stmt -> RuntimeM ()
funS x = case x of
  SGlobal stmts -> applyAndCompose funD stmts $ do
    code <- getResult $ invokestatic entrypoint []
    return_ code
  SLocal decls stmts -> applyAndCompose funD decls (mapM_ funS stmts)
  SAssign id expr -> do
    val <- funE expr
    store id val
  SAssignArr id expr1 expr2 -> do
    ref <- load id
    ind <- funE expr1
    val <- funE expr2
    astore ref ind val
  SAssignFld ido idf expr -> do
    val <- funE expr
    ref <- load ido
    putfield idf val ref
  SIf expr stmt -> do
    VBool val <- funE expr
    if val then funS stmt else nop
  SIfElse expr stmt1 stmt2 -> do
    VBool val <- funE expr
    if val then funS stmt1 else funS stmt2
  -- Note that (once again) there is no lexical variable hiding, after one
  -- iteration of a loop all variables declared inside fall out of the scope,
  -- which means we can dispose them.
  SWhile expr stmt -> do
    VBool val <- funE expr
    -- This is a bit hackish, but there is no reason why it won't work and we
    -- want to apply normal chaining rules without too much ifology
    -- Note that GC works fine here because stmt creates heap objects in its own scope
    if val then funS $ SLocal [] [stmt, SWhile expr stmt] else nop
  SExpr expr -> funE expr >> nop
  SThrow expr -> do
    ref <- funE expr
    throw ref
  STryCatch stmt1 typ id stmt2 -> tryCatch (funS stmt1) id (funS stmt2)
  SReturn expr -> do
    val <- funE expr
    return_ val
  SReturnV -> return'_
  SEmpty -> nop
  _ -> error $ Err.unusedBranch x

-- Expressions
funE :: Expr -> RuntimeM PrimValue
funE x = case x of
  -- Using JVMM monadic bytecode here might be misleading but during
  -- compilation process we actually turn expression into series of
  -- statements
  EVar id -> load id
  ELitInt n -> do
    let val = (VInt $ fromInteger n)
    checkVal val
    return val
  ELitTrue -> return (VBool True)
  ELitFalse -> return (VBool False)
  ELitString str -> alloc (VString str)
  ELitChar c -> return (VChar c)
  ENull -> return ref0
  EAccessArr expr1 expr2 -> do
    ref <- funE expr1
    ind <- funE expr2
    aload ref ind
  EAccessFn expr id exprs -> do
    ref <- funE expr
    vals <- mapM funE exprs
    getResult $ invokevirtual id ref vals
  EAccessVar expr id -> do
    ref <- funE expr
    getfield id ref
  EApp id exprs -> do
    vals <- mapM funE exprs
    getResult $ invokestatic id vals
  ENewArr typ expr -> do
    len <- funE expr
    newarray typ len
  ENewObj typ ->
    newobject typ
  EUnary _ OuNot expr -> do
    VBool val <- funE expr
    return $ VBool (not val)
  EUnary _ OuNeg expr -> do
    VInt val <- funE expr
    return $ VInt (negate val)
  EBinary TString ObPlus expr1 expr2 -> do
    ref1 <- funE expr1
    ref2 <- funE expr2
    VString str1 <- deref ref1
    VString str2 <- deref ref2
    alloc $ VString (str1 ++ str2)
  EBinary TInt opbin expr1 expr2 -> do
    VInt val1 <- funE expr1
    VInt val2 <- funE expr2
    when (opbin `elem` [ObDiv, ObMod] && val2 == 0) $ throwError $ RError Err.zeroDivision
    return $ VInt $ case opbin of
      ObPlus -> val1 + val2
      ObMinus -> val1 - val2
      ObTimes -> val1 * val2
      ObDiv -> val1 `div` val2
      ObMod -> val1 `rem` val2 -- I'mma hipst'a!
      _ -> error $ Err.unusedBranch opbin
  EBinary TBool ObAnd expr1 expr2 -> do
    VBool val1 <- funE expr1
    case val1 of
      True -> funE expr2
      False -> return $ VBool False
  EBinary TBool ObOr expr1 expr2 -> do
    VBool val1 <- funE expr1
    case val1 of
      True -> return $ VBool True
      False -> funE expr2
  EBinary TBool opbin expr1 expr2 -> do
    val1 <- funE expr1
    val2 <- funE expr2
    return $ VBool $ case opbin of
      -- This works because of 'deriving (Eq, Ord)'
      -- Note that references handling is automatic
      ObEQU -> val1 == val2
      ObNEQ -> val1 /= val2
      ObLTH -> val1 < val2
      ObLEQ -> val1 <= val2
      ObGTH -> val1 > val2
      ObGEQ -> val1 >= val2
      _ -> error $ Err.unusedBranch opbin
  _ -> error $ Err.unusedBranch x


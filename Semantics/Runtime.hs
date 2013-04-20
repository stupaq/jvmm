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
import Syntax.AbsJvmm (Ident(..), Expr(..), Stmt(..), OpBin(..), OpUn(..), Type(..))
import Semantics.Commons
import Semantics.Trans (UIdent(..), toStr, tempIdent)
import qualified Semantics.Errors as Err
import Semantics.Errors (rethrow)
import qualified Semantics.Scope as Scope

-- Implements runtime semantics and memory model, not that all type magic
-- should be moved to type cheking phase. Runtime polimorphism (virtual
-- functions) requires knowledge of runtime type.

-- BUILTINS --
--------------
entrypoint = Scope.tagSymbol Scope.tag0 "main"

builtinGlobal = map (\(id, fun) -> (Scope.tagSymbol Scope.tag0 id, \vals -> fun vals >> nop)) [
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

-- DATA REPRESENTATION --
-------------------------
type Loc = Integer
-- Null location
loc0 = 0

data Composite = Composite {
  fields :: Map.Map Ident PrimValue,
  methods :: Map.Map Ident Stmt
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
type FuncEnv = Map.Map Ident Func
funcenv0 = Map.fromList builtinGlobal

data RunEnv = RunEnv {
  funcs :: FuncEnv
}
runenv0 = RunEnv {
  funcs = funcenv0
}

-- RUNTIME STATE --
-------------------
type StackFrame = Map.Map Ident PrimValue
stackframe0 = Map.empty

data RunState = RunState {
  stack :: StackFrame,
  heap :: Map.Map Loc RefValue
}
runstate0 = RunState {
  stack = stackframe0,
  -- Reference with location == 0 is a null reference
  heap = Map.singleton loc0 VNothing
}

-- RUNTIME MONAD --
-------------------
-- We need to be able to catch an exception without loosing the state -- this
-- is why we have to reverse the order of ErrorT and StateT
-- Lack of a stack in ReaderT monad (compare with StateT) is caused by the fact
-- that we will never have to inspect hidden part of it
type RuntimeM = ReaderT RunEnv (ErrorT Result (StateT RunState IO))
runRuntimeM :: RunEnv -> RunState -> RuntimeM a -> IO (Either Result a, RunState)
runRuntimeM r s m = runStateT (runErrorT (runReaderT m r)) s

-- Executes given action in a new 'stack frame', restores old one afterwards
-- The assumption that there is no syntactic hiding is EXTREMELY strong and
-- important, we can push new stack frame only when we CALL a function
-- ACHTUNG once again, we do NOT use newFrame outside of a function call!
newFrame :: RuntimeM a -> RuntimeM a
newFrame action = do
  st <- gets stack
  tryFinally action $
    -- Pop stack frame no matter what happened (exception/error/return)
    modify (\state -> state { stack = st })

getResult :: RuntimeM () -> RuntimeM PrimValue
getResult action = do
  (action  >> return VVoid) `catchError` handler
  where
    handler :: Result -> RuntimeM PrimValue
    handler err = case err of
      RValue val -> return val
      _ -> throwError err

tryCatch :: RuntimeM () -> Ident -> RuntimeM () -> RuntimeM ()
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
-- FIXME embed these in all monadic bytecode instructions
-- If one uses bytecode monadic instruction only the garbage collection is
-- automatic, these functions should not be invoked directly.
alloc :: RefValue -> RuntimeM PrimValue
alloc val = do
  loc <- gets freeloc
  let ref = VRef loc
  update ref val
  return ref
  where
    freeloc :: RunState -> Loc
    freeloc = (+1) . fst . Map.findMax . heap -- FIXME this is imperfect if we have GC

-- This is necessary for reference counting
dupl :: PrimValue -> RuntimeM PrimValue
dupl (VRef 0) = return (VRef 0)
dupl (VRef loc) = return (VRef loc) -- FIXME

-- Argument can only be a reference (might be a null one)
free :: PrimValue -> RuntimeM ()
free (VRef loc) = return () -- FIXME

deref :: PrimValue -> RuntimeM RefValue
deref (VRef 0) = throwError (RError Err.nullPointerException)
deref (VRef loc) = gets (Maybe.fromJust . Map.lookup loc . heap)

update :: PrimValue -> RefValue -> RuntimeM ()
update (VRef loc) val = modify (\state -> state { heap = Map.insert loc val (heap state) })

-- JVMM bytecode --
-------------------
-- One can obtain Jasmin mnemonics by prepending type indicator, and getting
-- arguments from the stack
nop :: RuntimeM ()
nop = return ()

load :: Ident -> RuntimeM PrimValue
load id = gets (Maybe.fromJust . Map.lookup id . stack)

aload :: PrimValue -> PrimValue -> RuntimeM PrimValue
aload ref (VInt ind) = do
  VArray arr <- deref ref
  case Map.lookup ind arr of
    Nothing -> throwError (RError $ Err.indexOutOfBounds ind)
    Just val -> return val

store :: Ident -> PrimValue -> RuntimeM ()
store id val = modify $ \state -> state { stack = Map.insert id val (stack state) }

astore :: PrimValue -> PrimValue -> PrimValue -> RuntimeM ()
astore ref (VInt ind) val = do
  VArray arr <- deref ref
  unless (0 <= ind && ind < Map.size arr) $ throwError (RError $ Err.indexOutOfBounds ind)
  update ref (VArray $ Map.insert ind val arr)

invokestatic :: Ident -> [PrimValue] -> RuntimeM ()
invokestatic id vals = asks (Maybe.fromJust . Map.lookup id . funcs) >>= ($ vals)

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

-- TODO classes and stuff
getfield :: Ident -> PrimValue -> RuntimeM PrimValue
getfield (Ident "length$0") ref = do
  arraylength ref

invokevirtual :: Ident -> PrimValue -> [PrimValue] -> RuntimeM ()
invokevirtual (Ident "charAt$0") ref [VInt ind] = do
  VString str <- deref ref
  unless (0 <= ind && ind < length str) $ throwError (RError $ Err.indexOutOfBounds ind)
  return_ $ VChar (head $ drop ind str)
-- TODO

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
      -- Main function shoul have integere return value
      let VInt ret = res
      return ret

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
      -- same code as in funS (Local ...)
      funS $ Local args ((map (\(SDeclVar _ id) -> SAssign id (EVar $ argId id)) args) ++ [stmt])
      -- In case function does not return explicitly we do it here with default value
      val <- defaultValue typ
      return_ val
        where
          argId id = tempIdent id "arg"
funD (SDeclVar typ id) = (>>) (defaultValue typ >>= store id)

-- Statements
funS :: Stmt -> RuntimeM ()
funS x = case x of
  Global stmts -> applyAndCompose funD stmts $ invokestatic entrypoint []
  Local decls stmts -> applyAndCompose funD decls (mapM_ funS stmts)
  SAssign id expr -> do
    val <- funE expr
    store id val
  SAssignArr id expr1 expr2 -> do
    ref <- load id
    ind <- funE expr1
    val <- funE expr2
    astore ref ind val
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
    if val then funS $ Local [] [stmt, SWhile expr stmt] else nop
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

-- Expressions
funE :: Expr -> RuntimeM PrimValue
funE x = case x of
  -- Using JVMM monadic bytecode here might be misleading but during
  -- compilation process we actually turn expression into series of
  -- statements
  EVar id -> load id
  ELitInt n -> return (VInt $ fromInteger n)
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
    val <- funE expr
    newarray typ val
  EUnaryT _ Not expr -> do
    VBool val <- funE expr
    return $ VBool (not val)
  EUnaryT _ Neg expr -> do
    VInt val <- funE expr
    return $ VInt (negate val)
  EBinaryT TString Plus expr1 expr2 -> do
    ref1 <- funE expr1
    ref2 <- funE expr2
    VString str1 <- deref ref1
    VString str2 <- deref ref2
    free ref1
    free ref2
    alloc $ VString (str1 ++ str2)
  EBinaryT TInt opbin expr1 expr2 -> do
    VInt val1 <- funE expr1
    VInt val2 <- funE expr2
    return $ VInt $ case opbin of
      Plus -> val1 + val2
      Minus -> val1 - val2
      Times -> val1 * val2
      Div -> val1 `div` val2
      Mod -> val1 `mod` val2
  EBinaryT TBool And expr1 expr2 -> do
    VBool val1 <- funE expr1
    VBool val2 <- funE expr2
    return $ VBool (val1 && val2)
  EBinaryT TBool Or expr1 expr2 -> do
    VBool val1 <- funE expr1
    VBool val2 <- funE expr2
    return $ VBool (val1 || val2)
  EBinaryT TBool opbin expr1 expr2 -> do
    val1 <- funE expr1
    val2 <- funE expr2
    return $ VBool $ case opbin of
      -- This works because of 'deriving (Eq, Ord)'
      -- Note that references handling is automatic
      EQU -> val1 == val2
      NEQ -> val1 /= val2
      LTH -> val1 < val2
      LEQ -> val1 <= val2
      GTH -> val1 > val2
      GEQ -> val1 >= val2


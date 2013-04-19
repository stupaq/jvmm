module Semantics.Runtime where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Semantics.Builtins as Builtins
import qualified Semantics.Scope as Scope
import qualified Semantics.Errors as Err
import Semantics.Errors (rethrow)
import Semantics.Trans (UIdent(..), toStr)
import Syntax.AbsJvmm (Ident(..), Expr(..), Stmt(..), OpBin(..), OpUn(..), Type(..))

-- FIXME import it from somewhere
-- FIXME remove nops
-- TODO this is hackish, fix it when the time comes
semantics = map (\(id, fun) -> (Ident id, \vals -> fun vals >> nop)) [
  ("printInt$0",
      \[VInt val] -> do
        liftIO $ putStrLn (show val)
        return VVoid
  ),
  ("readInt$0",
      \[] -> do
        val <- liftIO (readLn :: IO Int)
        return $ VInt val
  ),
  ("printString$0",
      \[ref] -> do
        VString str <- deref ref
        liftIO (putStrLn str)
        return VVoid
  ),
  ("readString$0",
      \[] -> do
        val <- liftIO (readLn :: IO String)
        return $ VString val
  ),
  ("error$0",
      \[] -> throwError $ VError Err.userError
  )]

-- Implements runtime semantics and memory model, not that all type magic
-- should be moved to type cheking phase. Runtime polimorphism (virtual
-- functions) requires knowledge of runtime type.

-- DATA REPRESENTATION --
-------------------------
type Loc = Integer
-- Null location
loc0 = 0

data Composite = Composite {
  fields :: Map.Map Ident Value,
  methods :: Map.Map Ident Stmt
} deriving (Eq, Ord, Show)
composite0 = Composite {
  fields = Map.empty,
  methods = Map.empty
}

type Array = Map.Map Int Value

data Value =
  -- Things stored on stack and passed by value
    VInt Int
  | VChar Char
  | VBool Bool
  | VRef Loc
  -- Things stored on heap are only referenced
  | VArray Array
  | VString String
  | VObject Composite
  -- For a function returning void, this partially solves Result problem
  | VVoid
  -- This is an exception, which can be thrown and caught by a user
  | VException Value
  -- This error can be thrown only by an interpreter itself when irrecoverable
  -- error occurs (null pointer exception, zero division etc.)
  | VError String
  deriving (Eq, Ord, Show)
-- Null reference
ref0 = VRef loc0

instance Error Value where
  strMsg s = VError s

-- Default values of each type
defaultValue :: Type -> RuntimeM Value
defaultValue typ = return $ case typ of
  TInt -> VInt 0
  TChar -> VChar '\0' -- FIXME this is so fucking dumb I can't handle this
  TBool -> VBool False
  TString -> ref0
  TObject -> ref0
  TArray _ -> ref0
  TUser _ -> ref0
  TVoid -> VVoid

-- STATIC ENVIRONMENT --
------------------------
type Func = [Value] -> RuntimeM Result
type FuncEnv = Map.Map Ident Func
funcenv0 = Map.fromList semantics

data RunEnv = RunEnv {
  funcs :: FuncEnv
}
runenv0 = RunEnv {
  funcs = funcenv0
}

-- RUNTIME STATE --
-------------------
type StackFrame = Map.Map Ident Value
stackframe0 = Map.empty

data RunState = RunState {
  stack :: StackFrame,
  heap :: Map.Map Loc Value
}
runstate0 = RunState {
  stack = stackframe0,
  -- Reference with location == 0 is a null reference
  heap = Map.singleton loc0 VVoid
}

-- RUNTIME MONAD --
-------------------
-- We need to be able to catch an exception without loosing the state -- this
-- is why we have to reverse the order of ErrorT and StateT
-- Lack of a stack in ReaderT monad (compare with StateT) is caused by the fact
-- that we will never have to inspect hidden part of it
type RuntimeM = ReaderT RunEnv (ErrorT Value (StateT RunState IO))
runRuntimeM :: RunEnv -> RunState -> RuntimeM a -> IO (Either Value a, RunState)
runRuntimeM r s m = runStateT (runErrorT (runReaderT m r)) s

-- FIXME this is no enough, return should skip statements just like error does
type Result = () -- FIXME figure out how to pass result of a Stmt
result0 = ()

runInterpreter :: IO ((Either Value Result, RunState)) -> IO ()
runInterpreter = (=<<) $ \(x, state) -> case x of
    Left err -> ioError $ userError (show err)
    Right res -> do
      putStrLn $ "Exit code:\t" ++ (show res)
      putStrLn $ "Heap stats:\n\tallocated_count:\t" ++ (show $ Map.size (heap state Map.\\ heap runstate0))
      return ()

-- Executes _int main()_ function in given translation unit
runUnit :: Stmt -> IO ((Either Value Result, RunState))
runUnit = runRuntimeM runenv0 runstate0 . funS

-- Executes given action in a new 'stack frame', restores old one afterwards
-- The assumption that there is no syntactic hiding is EXTREMELY strong and
-- important, we can push new stack frame only when we CALL a function
-- ACHTUNG once again, we do NOT use newFrame outside of a function call!
newFrame :: RuntimeM a -> RuntimeM a
newFrame action = do
  st <- gets stack
  res <- action
  modify (\state -> state { stack = st })
  return res

-- MEMORY MODEL --
------------------
-- FIXME embed these in all monadic bytecode instructions
-- One cannot allocate reference or primitive value on the heap
alloc :: Value -> RuntimeM Value
-- FIXME enforce above
alloc val = do
  loc <- gets freeloc
  let ref = VRef loc
  update ref val
  return ref
  where
    freeloc :: RunState -> Loc
    freeloc = (+1) . fst . Map.findMax . heap -- FIXME this is imperfect if we have GC

-- This is necessary for reference counting
dupl :: Value -> RuntimeM Value
dupl (VRef 0) = return (VRef 0)
dupl (VRef loc) = return (VRef loc) -- FIXME

-- Argument can only be a reference (might be a null one)
free :: Value -> RuntimeM ()
free (VRef loc) = return () -- FIXME

deref :: Value -> RuntimeM Value
deref (VRef 0) = throwError (VError Err.nullPointerException)
deref (VRef loc) = gets (Maybe.fromJust . Map.lookup loc . heap)

update :: Value -> Value -> RuntimeM ()
update (VRef loc) val = modify (\state -> state { heap = Map.insert loc val (heap state) })

-- JVMM bytecode --
-------------------
-- One can obtain Jasmin mnemonics by prepending type indicator, and getting
-- arguments from the stack
nop :: RuntimeM Result
nop = return result0

load :: Ident -> RuntimeM Value
load id = gets (Maybe.fromJust . Map.lookup id . stack)

aload :: Value -> Value -> RuntimeM Value
aload ref (VInt ind) = do
  VArray arr <- deref ref
  case Map.lookup ind arr of
    Nothing -> throwError (VError $ Err.indexOutOfBounds ind)
    Just val -> return val

store :: Ident -> Value -> RuntimeM ()
store id val = modify $ \state -> state { stack = Map.insert id val (stack state) }

astore :: Value -> Value -> Value -> RuntimeM ()
astore ref (VInt ind) val = do
  VArray arr <- deref ref
  unless (0 <= ind && ind < Map.size arr) $ throwError (VError $ Err.indexOutOfBounds ind)
  update ref (VArray $ Map.insert ind val arr)

invokestatic :: Ident -> [Value] -> RuntimeM Result
invokestatic id vals = asks (Maybe.fromJust . Map.lookup id . funcs) >>= ($ vals)

throw :: Value -> RuntimeM Result
throw = throwError . VException

newarray :: Type -> Value -> RuntimeM Value
newarray typ (VInt len) = do
  val <- defaultValue typ
  alloc $ VArray $ foldl (\m i -> Map.insert i val m) Map.empty [0..(len - 1)]

arraylength :: Value -> RuntimeM Value
arraylength ref = deref ref >>= (\(VArray arr) -> return $ VInt $ Map.size arr)

-- TODO classes and stuff
getfield :: Ident -> Value -> RuntimeM Value
getfield = undefined

invokevirtual :: Ident -> Value -> [Value] -> RuntimeM Value
invokevirtual = undefined
-- TODO

-- DENOTATIONAL SEMANTICS --
----------------------------

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
      zipWithM_ (\(SDeclVar _ id) var -> store id var) args vals
      -- This is a hack, but it's extremenly refined one, we get garbage
      -- collection and all shit connected with that for free, it's exactly the
      -- same code as in funS (Local ...)
      funS $ Local args ((map (\(SDeclVar _ id) -> SAssign id (EVar id)) args) ++ [stmt])
funD (SDeclVar typ id) = (>>) (defaultValue typ >>= store id)

-- Statements
funS :: Stmt -> RuntimeM Result
funS x = case x of
  Global stmts -> compose (map funD stmts) $ do
    invokestatic Builtins.entrypoint []
  Local decls stmts -> compose (map funD decls) (mapM_ funS stmts)
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
  SWhile expr stmt -> do
    nop -- FIXME
  SForeach typ id expr stmt -> do
    nop -- FIXME
  SExpr expr -> funE expr >> nop
  SThrow expr -> do
    ref <- funE expr
    throw ref
  STryCatch stmt1 typ id stmt2 -> do
    funS stmt1 `catchError` handler
    where
      handler :: Value -> RuntimeM Result
      handler err = case err of
        VError _ -> throwError err
        VException val -> do
          store id val
          funS stmt2
  SReturn expr -> nop -- FIXME
  SReturnV -> nop -- FIXME
  SEmpty -> nop
  where
    compose :: [a -> a] -> a -> a
    compose fs = foldl (flip (.)) Prelude.id fs

-- Expressions
funE :: Expr -> RuntimeM Value
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
    aload ref ind -- FIXME
  EAccessFn expr id exprs -> do
    ref <- funE expr
    vals <- mapM funE exprs
    invokevirtual id ref vals
  EAccessVar expr id -> do
    ref <- funE expr
    getfield id ref
  EApp id exprs -> do
    vals <- mapM funE exprs
    invokestatic id vals
    return VVoid -- FIXME
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
  EBinaryT TBool And expr1 expr2 -> do
    VBool val1 <- funE expr1
    VBool val2 <- funE expr2
    return $ VBool (val1 && val2)
  EBinaryT TBool Or expr1 expr2 -> do
    VBool val1 <- funE expr1
    VBool val2 <- funE expr2
    return $ VBool (val1 || val2)


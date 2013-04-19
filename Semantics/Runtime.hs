module Semantics.Runtime where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Semantics.Builtins as Builtins
import qualified Semantics.Scope as Scope
import qualified Semantics.Errors as Err
import Semantics.Errors (rethrow)
import Semantics.Trans (UIdent(..), toStr)
import Syntax.AbsJvmm (Ident, Expr(..), Stmt(..), OpBin(..), OpUn(..), Type(..))

-- Implements runtime semantics and memory model, not that all type magic
-- should be moved to type cheking phase. Runtime polimorphism (virtual
-- functions) requires knowledge of runtime type.

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
  -- Things stored on heap
  | VArray Array
  | VString String
  | VObject Composite
  -- For a function returning void, this partially solves Result problem
  | VVoid
-- This error can be thrown only by an interpreter itself, when irrecoverable
-- error occurs (null pointer exception, zero division etc.)
  | VError String
  deriving (Eq, Ord, Show)
-- Null reference
ref0 = VRef loc0

instance Error Value where
  strMsg s = VError s

-- Visible part of a stack (without variables hidden by function calls)
data RunEnv = RunEnv {
  vars :: Map.Map Ident Value,
  funcs :: Map.Map Ident Stmt
}
runenv0 = RunEnv {
  vars = Map.empty,
  funcs = Map.empty -- FIXME add builtins
}

-- Every possible runtime error is represented by an exception, which cannot be
-- caught by user code
data RunState = RunState {
  -- Current stack
  stack :: RunEnv,
  heap :: Map.Map Loc Value
}
runstate0 = RunState {
  stack = runenv0,
  -- Reference with location == 0 is a null reference
  heap = Map.singleton loc0 VVoid
}

-- We need to be able to catch and exception without loosing the state -- this
-- is why we have to reverse the order of ErrorT and StateT
type RuntimeM = ReaderT RunEnv (ErrorT Value (StateT RunState IO))
runRuntimeM :: RunEnv -> RunState -> RuntimeM a -> IO (Either Value a, RunState)
runRuntimeM r s m = runStateT (runErrorT (runReaderT m r)) s

type Result = () -- FIXME figure out how to pass result of a Stmt
runInterpreter :: IO ((Either Value Result, RunState)) -> IO ()
runInterpreter = (=<<) $ \(x, state) -> case x of
    Left err -> ioError $ userError (show err)
    Right res -> do
      putStrLn $ "Heap stats:\n\tallocated_count:\t" ++ (show $ Map.size (heap state))
      return ()

-- Default values of each type
defval :: Type -> RuntimeM Value
defval typ = return $ case typ of
  TInt -> VInt 0
  TChar -> VChar 'J' -- FIXME this is so fucking dumb I can't handle this
  TBool -> VBool False
  TString -> ref0
  TObject -> ref0
  TArray _ -> ref0
  TUser _ -> ref0
  TVoid -> VVoid

-- FIXME embed these in all monadic bytecode instructions
-- Heap management (returned values are references)
-- One cannot allocate reference or primitive value on the heap
alloc :: Value -> RuntimeM Value
-- FIXME enforce above
alloc val = do
  loc <- gets freeloc
  modify (\state -> state { heap = Map.insert loc val (heap state) })
  return $ VRef loc
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

-- JVMM bytecode instructions, monadic dialect
load :: Ident -> RuntimeM Value
load id = undefined

aload :: Value -> Int -> RuntimeM Value
aload ref ind = undefined

newarray :: Type -> Int -> RuntimeM Value
newarray typ len = undefined

invokestatic :: Ident -> [Value] -> RuntimeM Value
invokestatic id vals = undefined

getfield :: Ident -> Value -> RuntimeM Value
getfield = undefined -- TODO

invokevirtual :: Ident -> Value -> [Value] -> RuntimeM Value
invokevirtual = undefined -- TODO

-- Executes int main() function in given translation unit
runUnit :: Stmt -> IO ((Either Value Result, RunState))
runUnit = runRuntimeM runenv0 runstate0 . funS
  where
    funS :: Stmt -> RuntimeM Result
    funS x = case x of
      _ -> undefined
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
        VArray arr <- deref ref
        VInt ind <- funE expr2
        case Map.lookup ind arr of
          Nothing -> throwError (VError $ Err.indexOutOfBounds ind)
          Just val -> return val
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
      ENewArr typ expr -> do
        VInt val <- funE expr
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


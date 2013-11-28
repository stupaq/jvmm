{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Jvmm.Scope.Internal where

import Control.Exception (assert)
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Traversable as Traversable

import qualified Jvmm.Errors as Err
import Jvmm.Errors (rethrow, orReturn, finally, ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- SCOPE REPRESENTATION --
--------------------------
data Symbol =
    SField FieldName
  | SMethod MethodName
  | SType ClassName
  deriving (Show, Eq, Ord)

type Scope = Set.Set Symbol
scope0 = Set.empty

-- SCOPE STATE --
-----------------
type Variables = Map.Map VariableName VariableNum
variables0 = Map.empty

type DeclarationId = VariableNum
declarationid0 = variablenum0

data ScopeState = ScopeState {
    scopestateVariables :: Variables
  , scopestateNextId :: DeclarationId
} deriving (Show)
scopestate0 = ScopeState variables0 declarationid0

-- SCOPE ENVIRONMENT --
-----------------------
data ScopeEnv = ScopeEnv {
    scopeenvInstance :: Scope
  , scopeenvStatic :: Scope
  , scopeenvParent :: Variables
} deriving (Show)
scopeenv0 = ScopeEnv scope0 scope0 variables0

-- SCOPE MONAD --
-----------------
type ScopeM = WriterT [Variable] (StateT ScopeState (ReaderT ScopeEnv (ErrorInfoT Identity)))
runScopeM :: ScopeM a -> ErrorInfoT Identity ((a, [Variable]), ScopeState)
runScopeM m = runReaderT (runStateT (runWriterT m) scopestate0) scopeenv0

lookupM :: (Show a, Ord a) => a -> Map.Map a b -> ScopeM b
lookupM key map = case Map.lookup key map of
  Nothing -> throwError $ Err.unboundSymbol key
  Just val -> return val

containsM :: (Show a, Ord a) => a -> Set.Set a -> ScopeM ()
containsM key set = guard (Set.member key set) `rethrow` Err.unboundSymbol key

-- SCOPE SWITCHING --
---------------------
-- Pushes scope 'stack frame', runs action in it, restores 'stack frame'
newLocalScope :: ScopeM a -> ScopeM a
newLocalScope action = do
  parent <- gets scopestateVariables
  local (\env -> env { scopeenvParent = parent }) action
      `finally` modify (\st -> st { scopestateVariables = parent })

-- SCOPE COLLECTING --
----------------------
enterClass :: Class -> ScopeM a -> ScopeM a
enterClass clazz = do
  let instanceScope = Set.fromList $ List.map (SField . fieldName) (classFields clazz)
        ++ List.map (SMethod . methodName) (classMethods clazz)
  let staticScope = Set.fromList $ List.map (SMethod . methodName) (classStaticMethods clazz)
  local (\env@ScopeEnv { scopeenvStatic = classes } -> env {
          scopeenvInstance = instanceScope
        , scopeenvStatic = classes `Set.union` staticScope
      })

enterMethod :: Method -> ScopeM Method -> ScopeM Method
enterMethod method@Method { methodArgs = args, methodName = name } action = do
  args' <- mapM rewriteArgument args `rethrow` Err.duplicateArg name
  method' <- action
  modify (\st -> st { scopestateNextId = declarationid0 })
  return method' { methodArgs = args' }
  where
    rewriteArgument :: Variable -> ScopeM Variable
    rewriteArgument var@Variable { variableName = name } = do
      num' <- declare name
      return var { variableNum = num' }

enterHierarchy :: ClassHierarchy -> ScopeM a -> ScopeM a
enterHierarchy hierarchy = do
  let classes = execWriter $ Traversable.mapM (\x -> tell [SType $ className x]) hierarchy
  local (\env -> env { scopeenvStatic = Set.fromList classes })

-- SCOPE QUERING --
------------------------------
class Resolvable a where
  dynamic :: a -> ScopeM ()
  dynamic = static
  static :: a -> ScopeM ()
  static = dynamic
  tryDynamic, tryStatic :: a -> ScopeM Bool
  tryDynamic x = (dynamic x >> return True) `orReturn` False
  tryStatic x = (static x >> return True) `orReturn` False

instance Resolvable MethodName where
  dynamic name = asks scopeenvInstance >>= containsM (SMethod name)
  static name = asks scopeenvStatic >>= containsM (SMethod name)

instance Resolvable FieldName where
  dynamic name = asks scopeenvInstance >>= containsM (SField name)

instance Resolvable ClassName where
  static name = asks scopeenvStatic >>= containsM (SType name)

instance Resolvable Type where
  static (TUser name) = static name
  static _ = return ()

checkRedeclaration :: VariableName -> ScopeM ()
checkRedeclaration name = do
  upper <- current name `orReturn` variablenumNone
  current <- parent name `orReturn` variablenumNone
  unless (upper == current) $ throwError (Err.redeclaredSymbol name)

isField :: VariableName -> ScopeM Bool
isField name = do
  var <- tryCurrent name
  field <- tryDynamic (fieldFromVariable name)
  guard (field || var) `rethrow` Err.unboundSymbol name
  return $ field && not var

isStatic :: MethodName -> ScopeM Bool
isStatic name = do
  inst <- tryDynamic name
  static <- tryStatic name
  guard (static || inst) `rethrow` Err.unboundSymbol name
  return $ static && not inst

-- SCOPE UPDATING --
--------------------
class Redeclarable a where
  declare :: a -> ScopeM DeclarationId
  current :: a -> ScopeM DeclarationId
  parent :: a -> ScopeM DeclarationId
  tryCurrent, tryParent :: a -> ScopeM Bool
  tryCurrent x = (current x >> return True) `orReturn` False
  tryParent x = (parent x >> return True) `orReturn` False

instance Redeclarable VariableName where
  declare name = do
    checkRedeclaration name
    free <- gets scopestateNextId
    modify (\st -> st {
          scopestateVariables = Map.insert name free (scopestateVariables st)
        , scopestateNextId = free + 1
      })
    current name
  current name = gets scopestateVariables >>= lookupM name
  parent name = asks scopeenvParent >>= lookupM name

-- SCOPE COMPUTATION --
-----------------------
funH :: ClassHierarchy -> ScopeM ClassHierarchy
funH = Traversable.mapM $ \clazz@(Class typ super fields methods staticMethods loc) -> 
  Err.withLocation loc $ do
    static typ
    static super
    enterClass clazz $ do
      mapM funF fields
      methods' <- mapM funM methods
      staticMethods' <- mapM funMS staticMethods
      return $ clazz {
            classMethods = methods'
          , classStaticMethods = staticMethods'
        }

funF :: Field -> ScopeM Field
funF field@(Field typ name origin) = do
  static typ
  dynamic name
  dynamic origin
  return field

funM :: Method -> ScopeM Method
funM x = do
  num <- declare (VariableName "self")
  assert (num == variablenum0) (return ())
  funMS x

funMS :: Method -> ScopeM Method
funMS method@(Method typ name _ stmt origin loc []) =
  Err.withLocation loc $ do
    dynamic name
    static typ
    dynamic origin
    enterMethod method $ do
      (stmt', vars') <- listen $ newLocalScope (funS stmt)
      return $ method {
            methodBody = stmt'
          , methodVariables = vars'
        }

funS :: Stmt -> ScopeM Stmt
funS x = case x of
  SBlock stmts -> do
    stmts' <- newLocalScope (mapM funS stmts)
    return $ SBlock stmts'
  SReturn expr -> do
    expr' <- funE expr
    return $ SReturn expr'
  SIf expr stmt -> do
    stmt' <- funS stmt
    expr' <- funE expr
    return $ SIf expr' stmt'
  SIfElse expr stmt1 stmt2 -> do
    stmt1' <- funS stmt1
    stmt2' <- funS stmt2
    expr' <- funE expr
    return $ SIfElse expr' stmt1' stmt2'
  SWhile expr stmt -> do
    expr' <- funE expr
    stmt' <- funS stmt
    return $ SWhile expr' stmt'
  SExpr expr -> do
    expr' <- funE expr
    return $ SExpr expr'
  SThrow expr -> do
    expr' <- funE expr
    return $ SThrow expr'
  SReturnV -> return SReturnV
  SEmpty -> return SEmpty
  SBuiltin -> return SBuiltin
  SInherited -> return SInherited
  SMetaLocation loc stmts -> stmtMetaLocation loc $ mapM funS stmts
  -- We replace all assignments that actually refer to instance fields but give precedence to local
  -- variables (like in Java), to referencee hidden field self.<field_name> construct can be used
  T_SAssign name expr -> do
    expr' <- funE expr
    varOrField name (\num -> SStore num expr') (\name' -> SPutField variablenum0 name' expr')
  T_SAssignArr name expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    num <- current name
    return $ SStoreArray num expr1' expr2'
  T_SAssignFld name1 name2 expr2 -> do
    expr2' <- funE expr2
    num <- current name1
    dynamic name2
    return $ SPutField num name2 expr2'
  T_SDeclVar typ name -> do
    declare name
    static typ
    num <- current name
    tell [Variable typ num name]
    return SEmpty
  T_STryCatch stmt1 typ name stmt2 -> do
    stmt1' <- newLocalScope (funS stmt1)
    newLocalScope $ do
      -- Catch body can hide exception variable
      declare name
      num <- current name
      tell [Variable typ num name]
      stmt2' <- newLocalScope (funS stmt2)
      return $ STryCatch stmt1' typ num stmt2'

funE :: Expr -> ScopeM Expr
funE x = case x of
  T_EVar name -> varOrField name (\num -> ELoad num) (\fname -> EGetField ELoadThis fname)
  EArrayLoad expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    return $ EArrayLoad expr1' expr2'
  -- We replace all assignments that actually refer to instance method, we also give precedence to
  -- instance methods over static ones.
  EInvokeStatic name exprs -> do
    exprs' <- mapM funE exprs
    stat <- isStatic name
    case stat of
      True -> static name >> return (EInvokeStatic name exprs')
      False -> dynamic name >> return (EInvokeVirtual ELoadThis name exprs')
  EInvokeVirtual expr name exprs -> do
    expr' <- funE expr
    exprs' <- mapM funE exprs
    dynamic name
    return $ EInvokeVirtual expr' name exprs'
  EGetField expr name -> do
    expr' <- funE expr
    dynamic name
    return $ EGetField expr' name
  ENewArr typ expr -> do
    static typ
    expr' <- funE expr
    return $ ENewArr typ expr'
  ENewObj typ -> do
    static typ
    return $ ENewObj typ
  EUnary _ op expr -> do
    expr' <- funE expr
    return $ EUnary TUnknown op expr'
  EBinary _ op expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    return $ EBinary TUnknown op expr1' expr2'
  _ -> return x

-- HELPERS --
-------------
varOrField name ifVar ifField = do
  field <- isField name
  case field of
    False -> current name >>= (return . ifVar)
    True -> do
      current name
      let fname = fieldFromVariable name
      dynamic fname
      return $ ifField fname


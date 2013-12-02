{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
  | SType Type
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
enterHierarchy :: ClassHierarchy -> ScopeM a -> ScopeM a
enterHierarchy hierarchy = do
  let classes = execWriter $ Traversable.mapM (\x -> tell [SType $ classType x]) hierarchy
  local (\env -> env { scopeenvStatic = Set.fromList classes })

enterClass :: Class -> ScopeM a -> ScopeM a
enterClass clazz = do
  let instanceScope = Set.fromList $ List.map (SField . fieldName) (classFields clazz)
        ++ List.map (SMethod . methodName) (classMethods clazz)
  let staticScope = Set.fromList $ List.map (SMethod . methodName) (classStaticMethods clazz)
  local (\env@ScopeEnv { scopeenvStatic = classes } -> env {
          scopeenvInstance = instanceScope
        , scopeenvStatic = classes `Set.union` staticScope
      })

enterMethod :: ScopeM a -> ScopeM a
enterMethod action = newLocalScope action `finally` resetDeclarationId
  where
    resetDeclarationId :: ScopeM ()
    resetDeclarationId = modify (\st -> st { scopestateNextId = declarationid0 })

-- SCOPE QUERING --
------------------------------
class Resolvable a where
  dynamic :: a -> ScopeM ()
  static :: a -> ScopeM ()
  tryDynamic, tryStatic :: a -> ScopeM Bool
  tryDynamic x = (dynamic x >> return True) `orReturn` False
  tryStatic x = (static x >> return True) `orReturn` False

instance Resolvable MethodName where
  dynamic name = asks scopeenvInstance >>= containsM (SMethod name)
  static name = asks scopeenvStatic >>= containsM (SMethod name)

instance Resolvable FieldName where
  dynamic name = asks scopeenvInstance >>= containsM (SField name)

instance Resolvable Type where
  static typ@(TUser _) = asks scopeenvStatic >>= containsM (SType typ)
  static (TFunc rett argst excepts) = do
    static rett
    forM_ argst static
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
class Scopeable a where
  scope :: a -> ScopeM a

instance Scopeable ClassHierarchy where
  scope = Traversable.mapM $ \clazz@(Class typ super fields methods staticMethods loc) ->
    Err.withLocation loc $ do
      static typ
      static super
      enterClass clazz $ do
        mapM scope fields
        methods' <- mapM scopeInstance methods
        staticMethods' <- mapM scopeStatic staticMethods
        return $ clazz {
              classMethods = methods'
            , classStaticMethods = staticMethods'
          }

instance Scopeable Field where
  scope field@(Field typ name origin) = do
    static typ
    dynamic name
    static origin
    return field

scopeInstance :: Method -> ScopeM Method
scopeInstance method@Method { methodName = name, methodLocation = loc } =
  Err.withLocation loc . newLocalScope $ do
    num <- declare (VariableName "self")
    assert (num == variablenum0) (return ())
    dynamic name
    scope method

scopeStatic :: Method -> ScopeM Method
scopeStatic method@Method { methodName = name, methodLocation = loc } =
  Err.withLocation loc . newLocalScope $ do
    static name
    scope method

instance Scopeable Method where
  scope method@(Method typ name args stmt origin loc []) = do
    static typ
    static origin
    enterMethod $ do
      args' <- mapM declareArg args `rethrow` Err.duplicateArg name
      (stmt', vars') <- listen $ newLocalScope (scope stmt)
      return method {
            methodBody = stmt'
          , methodArgs = args'
          , methodVariables = vars'
        }
    where
      declareArg :: Variable -> ScopeM Variable
      declareArg var@Variable { variableName = name } = do
        num' <- declare name
        return var { variableNum = num' }

instance Scopeable Stmt where
  scope x = case x of
    SEmpty -> return SEmpty
    SBlock stmts -> do
      stmts' <- newLocalScope (mapM scope stmts)
      return $ SBlock stmts'
    -- Memory access
    SStore _ _ -> undefined
    SStoreArray _ _ _ -> undefined
    SPutField variablenum0 field expr -> do
      expr' <- scope expr
      dynamic field
      return $ SPutField variablenum0 field expr'
    SPutField _ _ _ -> undefined
    -- Control statements
    SReturn expr -> do
      expr' <- scope expr
      return $ SReturn expr'
    SReturnV -> return SReturnV
    SIf expr stmt -> do
      stmt' <- scope stmt
      expr' <- scope expr
      return $ SIf expr' stmt'
    SIfElse expr stmt1 stmt2 -> do
      stmt1' <- scope stmt1
      stmt2' <- scope stmt2
      expr' <- scope expr
      return $ SIfElse expr' stmt1' stmt2'
    SWhile expr stmt -> do
      expr' <- scope expr
      stmt' <- scope stmt
      return $ SWhile expr' stmt'
    SThrow expr -> do
      expr' <- scope expr
      return $ SThrow expr'
    STryCatch _ _ _ _ -> undefined
    -- Special function bodies
    SBuiltin -> return SBuiltin
    SInherited -> return SInherited
    SExpr expr -> do
      expr' <- scope expr
      return $ SExpr expr'
    -- Metainformation carriers
    SMetaLocation loc stmts -> stmtMetaLocation loc $ mapM scope stmts
    -- These statements will be replaced with ones caring more context in subsequent phases
    T_SDeclVar typ name -> do
      declare name
      static typ
      num <- current name
      tell [Variable typ num name]
      return SEmpty
    -- We replace all assignments that actually refer to instance fields but give precedence to local
    -- variables (like in Java), to referencee hidden field self.<field_name> construct can be used
    T_SAssign name expr -> do
      expr' <- scope expr
      varOrField name (\num -> SStore num expr') (\name' -> SPutField variablenum0 name' expr')
    T_SAssignArr name expr1 expr2 -> do
      expr1' <- scope expr1
      expr2' <- scope expr2
      num <- current name
      return $ SStoreArray num expr1' expr2'
    T_SAssignFld name field expr2 -> do
      expr2' <- scope expr2
      num <- current name
      -- The field name we see here cannot be verified without type information
      return $ SPutField num field expr2'
    T_STryCatch stmt1 typ name stmt2 -> do
      stmt1' <- newLocalScope (scope stmt1)
      newLocalScope $ do
        -- Catch body can hide exception variable
        declare name
        num <- current name
        tell [Variable typ num name]
        stmt2' <- newLocalScope (scope stmt2)
        return $ STryCatch stmt1' typ num stmt2'

instance Scopeable Expr where
  scope x = case x of
    -- Literals
    ENull -> return x
    ELitTrue -> return x
    ELitFalse -> return x
    ELitChar _ -> return x
    ELitString _ -> return x
    ELitInt _ -> return x
    -- Memory access
    ELoad _ -> undefined
    ELoadThis -> return ELoadThis
    EArrayLoad expr1 expr2 -> do
      expr1' <- scope expr1
      expr2' <- scope expr2
      return $ EArrayLoad expr1' expr2'
    EGetField expr field -> do
      expr' <- scope expr
      -- The field name we see here cannot be verified without type information
      return $ EGetField expr' field
    -- Method calls
    -- We replace all calls that actually refer to instance method, we also give precedence to
    -- instance methods over static ones.
    EInvokeStatic name exprs -> do
      exprs' <- mapM scope exprs
      stat <- isStatic name
      case stat of
        True -> static name >> return (EInvokeStatic name exprs')
        False -> dynamic name >> return (EInvokeVirtual ELoadThis name exprs')
    EInvokeVirtual expr name exprs -> do
      expr' <- scope expr
      exprs' <- mapM scope exprs
      -- The method name we see here cannot be verified without type information
      return $ EInvokeVirtual expr' name exprs'
    -- Object creation
    ENewObj typ -> do
      static typ
      return $ ENewObj typ
    ENewArr typ expr -> do
      static typ
      expr' <- scope expr
      return $ ENewArr typ expr'
    -- Operations
    EUnary _ op expr -> do
      expr' <- scope expr
      return $ EUnary TUnknown op expr'
    EBinary _ op expr1 expr2 -> do
      expr1' <- scope expr1
      expr2' <- scope expr2
      return $ EBinary TUnknown op expr1' expr2'
    -- These expressions will be replaced with ones caring more context in subsequent phases
    T_EVar name -> varOrField name (\num -> ELoad num) (\field -> EGetField ELoadThis field)

-- HELPERS --
-------------
varOrField name ifVar ifField = do
  field <- isField name
  case field of
    False -> current name >>= (return . ifVar)
    True -> do
      let field = fieldFromVariable name
      dynamic field
      return $ ifField field


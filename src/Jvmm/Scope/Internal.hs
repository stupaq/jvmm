{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DoAndIfThenElse      #-}
module Jvmm.Scope.Internal where

import Control.Applicative
import Control.Exception (assert)
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable

import Jvmm.Errors (ErrorInfoT, finally, orReturn, rethrow)
import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output

-- SCOPE REPRESENTATION --
--------------------------
data Symbol =
    SField FieldName
  | SMethod MethodName
  | SType TypeComposed
  deriving (Show, Eq, Ord)

type Scope = Set.Set Symbol

scope0 :: Scope
scope0 = Set.empty

-- SCOPE STATE --
-----------------
type Variables = Map.Map VariableName DeclarationId

variables0 :: Variables
variables0 = Map.empty

type DeclarationId = Int

declarationid0 :: DeclarationId
declarationid0 = 0

variablenum0 :: VariableNum
variablenum0 = VariableNum declarationid0

data ScopeState = ScopeState {
    scopestateVariables :: Variables
  , scopestateNextId    :: DeclarationId
} deriving (Show)

scopestate0 :: ScopeState
scopestate0 = ScopeState variables0 declarationid0

-- SCOPE ENVIRONMENT --
-----------------------
data ScopeEnv = ScopeEnv {
    scopeenvInstance :: Scope
  , scopeenvStatic   :: Scope
  , scopeenvParent   :: Variables
} deriving (Show)

scopeenv0 :: ScopeEnv
scopeenv0 = ScopeEnv scope0 scope0 variables0

-- SCOPE MONAD --
-----------------
type ScopeM = WriterT [Variable] (StateT ScopeState (ReaderT ScopeEnv (ErrorInfoT Identity)))
runScopeM :: ScopeM a -> ErrorInfoT Identity ((a, [Variable]), ScopeState)
runScopeM m = runReaderT (runStateT (runWriterT m) scopestate0) scopeenv0

lookupM :: (Show a, Ord a) => a -> Map.Map a b -> ScopeM b
lookupM key assoc = case Map.lookup key assoc of
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
        ++ List.map (SMethod . methodName) (classInstanceMethods clazz)
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
  -- Resolves reference dynamically (in local scope or instance scope)
  dynamic :: a -> ScopeM ()
  -- Resolves reference statically (in static scope)
  static :: a -> ScopeM ()
  tryDynamic, tryStatic :: a -> ScopeM Bool
  tryDynamic x = (dynamic x >> return True) `orReturn` False
  tryStatic x = (static x >> return True) `orReturn` False

instance Resolvable MethodName where
  dynamic name = asks scopeenvInstance >>= containsM (SMethod name)
  static name = asks scopeenvStatic >>= containsM (SMethod name)

instance Resolvable FieldName where
  dynamic name = asks scopeenvInstance >>= containsM (SField name)
  static = Err.unreachable

instance Resolvable Type where
  dynamic = Err.unreachable
  static (TMethod typ) = static typ
  static (TBasic typ) = static typ

instance Resolvable TypeMethod where
  dynamic = Err.unreachable
  static (TypeMethod rett argst excepts) = do
    static rett
    forM_ argst static
    forM_ excepts static

instance Resolvable TypeBasic where
  dynamic = Err.unreachable
  static (TPrimitive typ) = static typ
  static (TComposed typ) = static typ

instance Resolvable TypePrimitive where
  dynamic = Err.unreachable
  static _ = return ()

instance Resolvable TypeComposed where
  dynamic = Err.unreachable
  static typ@(TUser _) = asks scopeenvStatic >>= containsM (SType typ)
  static _ = return ()

checkRedeclaration :: VariableName -> ScopeM ()
checkRedeclaration name = do
  upper <- inCurrent name `orReturn` VariableThis
  current <- inParent name `orReturn` VariableThis
  unless (upper == current) $ throwError (Err.redeclaredSymbol name)

isField :: VariableName -> ScopeM Bool
isField name = do
  var <- tryCurrent name
  field <- tryDynamic (fieldFromVariable name)
  guard (field || var) `rethrow` Err.unboundSymbol name
  return $ field && not var

isStatic :: MethodName -> ScopeM Bool
isStatic name = do
  isInst <- tryDynamic name
  isClass <- tryStatic name
  guard (isClass || isInst) `rethrow` Err.unboundSymbol name
  return $ isClass && not isInst

-- SCOPE UPDATING --
--------------------
class Redeclarable a where
  -- Declares reference
  declare :: a -> ScopeM VariableNum
  -- Resolves reference in current scope
  inCurrent :: a -> ScopeM VariableNum
  -- Resolves reference in the scope from parent block (before entering current scope)
  inParent :: a -> ScopeM VariableNum
  tryCurrent, tryParent :: a -> ScopeM Bool
  tryCurrent x = (inCurrent x >> return True) `orReturn` False
  tryParent x = (inParent x >> return True) `orReturn` False

instance Redeclarable VariableName where
  declare name = do
    checkRedeclaration name
    free <- gets scopestateNextId
    modify (\st -> st {
          scopestateVariables = Map.insert name free (scopestateVariables st)
        , scopestateNextId = free + 1
      })
    inCurrent name
  inCurrent name = liftM VariableNum $ gets scopestateVariables >>= lookupM name
  inParent name = liftM VariableNum $ asks scopeenvParent >>= lookupM name

-- SCOPE COMPUTATION --
-----------------------
class Scopeable a where
  scope :: a -> ScopeM a

instance Scopeable ClassHierarchy where
  scope = Traversable.mapM $ \clazz@(Class typ super fields methods loc) ->
    Err.withLocation loc $ do
      static typ
      static super
      enterClass clazz $ do
        mapM_ scope fields
        methods' <- mapM scope methods
        return $ clazz { classAllMethods = methods' }

instance Scopeable Field where
  scope field@(Field typ name origin) = do
    static typ
    dynamic name
    static origin
    return field

instance Scopeable Method where
  scope method@(Method typ name args stmt origin loc _ inst) =
    Err.withLocation loc . newLocalScope $ do
      if inst
      then do
        num <- declare (VariableName "self")
        assert (num == variablenum0) $ return ()
        dynamic name
      else
        static name
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
      declareArg var@Variable { variableName = vname } = do
        num' <- declare vname
        return var { variableNum = num' }

instance Scopeable Stmt where
  scope x = case x of
    SEmpty -> return SEmpty
    SBlock stmts -> do
      stmts' <- newLocalScope (mapM scope stmts)
      -- Blocks have no semantic value after we compute scope
      case stmts' of
        [] -> return SEmpty
        [stmt'] -> return stmt'
        _ -> return $ SBlock stmts'
    -- Memory access
    SAssign lval expr typ -> SAssign <$> scope lval <*> scope expr <#> typ
    -- Control statements
    SReturn expr _ -> do
      expr' <- scope expr
      return $ SReturn expr' undef
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
    STryCatch {} -> Err.unreachable x
    -- Special function bodies
    SBuiltin -> return SBuiltin
    SInherited -> return SInherited
    SExpr expr _ -> do
      expr' <- scope expr
      return $ SExpr expr' undef
    -- Metainformation carriers
    SMetaLocation loc stmts -> stmtMetaLocation loc $ mapM scope stmts
    -- These statements will be replaced with ones caring more context in subsequent phases
    PruneSDeclVar typ name -> do
      declare name
      static typ
      num <- inCurrent name
      tell [Variable typ num name]
      return SEmpty
    PruneSTryCatch stmt1 typ name stmt2 -> do
      stmt1' <- newLocalScope (scope stmt1)
      newLocalScope $ do
        -- Catch body can hide exception variable
        declare name
        num <- inCurrent name
        tell [Variable (TComposed typ) num name]
        stmt2' <- newLocalScope (scope stmt2)
        return $ STryCatch stmt1' typ num stmt2'

instance Scopeable RValue where
  scope x = case x of
    -- Literals
    ENull _ -> return x
    ELitTrue -> return x
    ELitFalse -> return x
    ELitChar _ -> return x
    ELitString _ -> return x
    ELitInt _ -> return x
    -- Memory access
    ELoad VariableThis _ -> return x
    ELoad _ _ -> Err.unreachable x
    EArrayLoad expr1 expr2 _ -> do
      expr1' <- scope expr1
      expr2' <- scope expr2
      return $ EArrayLoad expr1' expr2' undef
    EGetField expr _ field _ -> do
      expr' <- scope expr
      -- The field name we see here cannot be verified without type information
      return $ EGetField expr' undef field undef
    -- Method calls
    -- We replace all calls that actually refer to instance method, we also give precedence to
    -- instance methods over static ones.
    EInvokeStatic _ name _ exprs -> do
      exprs' <- mapM scope exprs
      stat <- isStatic name
      if stat
      then static name >> return (EInvokeStatic undef name undef exprs')
      else dynamic name >>
          return (EInvokeVirtual (ELoad VariableThis undef) undef name undef exprs')
    EInvokeVirtual expr _ name _ exprs -> do
      expr' <- scope expr
      exprs' <- mapM scope exprs
      -- The method name we see here cannot be verified without type information
      return $ EInvokeVirtual expr' undef name undef exprs'
    -- Object creation
    ENewObj typ -> do
      static typ
      return $ ENewObj typ
    ENewArr typ expr -> do
      static typ
      expr' <- scope expr
      return $ ENewArr typ expr'
    -- Operations
    EUnary op expr _ -> do
      expr' <- scope expr
      return $ EUnary op expr' undef
    EBinary op expr1 expr2 _ _ _ -> do
      expr1' <- scope expr1
      expr2' <- scope expr2
      return $ EBinary op expr1' expr2' undef undef undef
    -- These expressions will be replaced with ones caring more context in subsequent phases
    PruneEVar name ->
      varOrField name
        (`ELoad` undef)
        (\field -> EGetField (ELoad VariableThis undef) undef field undef)
    PruneENull -> return x

instance Scopeable LValue where
  scope lval = do
    lval' <- fmap toLValue $ scope $ toRValue lval
    if isPureLValue lval'
    then return lval'
    else throwError $ Err.expressionIsNotLValue lval'

-- HELPERS --
-------------
varOrField :: VariableName -> (VariableNum -> b) -> (FieldName -> b) -> ScopeM b
varOrField name ifVar ifField = do
  aField <- isField name
  if aField
  then do
      let field = fieldFromVariable name
      dynamic field
      return $ ifField field
  else liftM ifVar (inCurrent name)


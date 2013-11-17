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
import Jvmm.Errors (rethrow, finally, ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- SCOPE REPRESENTATION --
--------------------------
type Tag = Int
tag0 = 0 :: Tag

tagWith :: Tag -> UIdent -> UIdent
tagWith tag = (+/+ concat ["$", show tag])

-- Each symbol in scoped tree has an identifier which is unique in its scope,
-- in other words, there is no identifier hiding in scoped tree.
type Scope = Map.Map UIdent Tag

scope0 = Map.empty

newOccurence :: UIdent -> Scope -> Scope
newOccurence id = Map.insertWith (\_ -> (+ 1)) id tag0

resolve :: UIdent -> Scope -> (ErrorInfoT Identity) UIdent
resolve id sc = case Map.lookup id sc of
  Just tag -> return $ tagWith tag id
  Nothing -> throwError $ Err.unboundSymbol id

-- SCOPE ENVIRONMENT --
-----------------------
data ScopeEnv = ScopeEnv {
    scopeenvInstance :: Scope
  , scopeenvStatic :: Scope
  , scopeenvParent :: Scope
} deriving (Show)

scopeenv0 = ScopeEnv scope0 scope0 scope0

-- We cannot resolve scope here without type information
scopeenvForeign :: UIdent -> ScopeEnv -> Scope
scopeenvForeign member _ = let scope1 = newOccurence member scope0 in
  case member of
    TIdent _ -> error $ Err.unusedBranch "scopeenvForeign asked for member type"
    _ -> scope1

scopeenvEmpty, scopeenvFull :: ScopeEnv -> Scope
scopeenvEmpty = const scope0
-- This prefers non-static symbols and this is what we want
scopeenvFull env = Map.union (scopeenvInstance env) (scopeenvStatic env)

-- SCOPE MONAD --
-----------------
type ScopeM = StateT Scope (ReaderT ScopeEnv (ErrorInfoT Identity))
runScopeM :: ScopeM a -> ErrorInfoT Identity (a, Scope)
runScopeM m = runReaderT (runStateT m scope0) scopeenv0

resolveLocal, resolveInstance, resolveParent :: UIdent -> ScopeM UIdent
resolveLocal id = (get) >>= (lift . lift . (resolve id))
resolveInstance id = (asks scopeenvInstance) >>= (lift . lift . (resolve id))
resolveStatic id = (asks scopeenvStatic) >>= (lift . lift . (resolve id))
resolveParent id = (asks scopeenvParent) >>= (lift . lift . (resolve id))

checkRedeclaration:: UIdent -> ScopeM ()
checkRedeclaration id = do
  -- resolve in block's beginning's scope
  idout <- resolveParent id `catchError` (\_ -> return id)
  -- resolve in current scope
  idin <- resolveLocal id `catchError` (\_ -> return id)
  unless (idin == idout) $ throwError $ Err.redeclaredSymbol id

-- SCOPE SWITCHING --
---------------------
-- Pushes scope 'stack frame', runs action in it, restores 'stack frame'
newLocal action = do
  parent <- get
  (local (\env -> env { scopeenvParent = parent }) action) `finally` (put parent)

-- Sets current scope fromScopeEnv via given accessor
asCurrent :: (ScopeEnv -> Scope) -> ScopeM a -> ScopeM a
asCurrent query action = do
  saved <- get
  (asks query >>= put >> action) `finally` (put saved)

-- Returns current scope
getCurrent :: ScopeM Scope
getCurrent = get

-- SCOPE COLLECTING --
----------------------
enterClass :: Class -> ScopeM a -> ScopeM a
enterClass clazz action = do
  global <- getCurrent
  instanceScope <- const global `asCurrent` do
    mapM_ (\Field { fieldIdent = id } -> decVar id) $ classFields clazz
    mapM_ (\Method { methodIdent = id } -> decFunc id) $ classMethods clazz
    getCurrent
  staticScope <- const global `asCurrent` do
    mapM_ (\Method { methodIdent = id } -> decFunc id) $ classStaticMethods clazz
    getCurrent
  -- ASSERTION
  (getCurrent >>= return . (flip assert ()) . (== global))
  -- This isolates scopes from affecting global one, no change could happen
  -- We will change scope do so for each method type (static, instance) separately
  local (\env -> env {
          scopeenvInstance = instanceScope
        , scopeenvStatic = staticScope
      }) action

collectClasses :: ClassHierarchy -> ScopeM ()
collectClasses classes = Traversable.mapM (decType . classType) classes >> return ()

-- SCOPE READING --
-------------------
resVar, resFunc :: UIdent -> ScopeM UIdent
resVar id = case id of
  IThis -> return id
  _ -> resolveLocal id
resFunc = resolveLocal
-- We do not support types hiding (but we want to check for redeclarations and usage of undeclared types)
resType :: Type -> ScopeM Type
resType typ = case typ of
  TUser id -> (resolveLocal id) >> (return typ)
  TFunc ret args excs -> liftM3 TFunc (resType ret) (mapM resType args) (mapM resType excs)
  _ -> return typ

isField :: UIdent -> ScopeM Bool
isField id@(VIdent _) = do
  -- resolve in global scope
  idout <- resolveInstance id `catchError` (\_ -> return id)
  -- resolve in current scope
  idin <- resolveLocal id `catchError` (\_ -> return id)
  return (idout == idin)
isField _ = error $ Err.unusedBranch "checking whether not a variable identifier is field"

isStatic :: UIdent -> ScopeM Bool
isStatic id@(FIdent _) = (resolveStatic id >> return False) `catchError` (\_ -> return False)
isStatic _ = error $ Err.unusedBranch "checking whether not a function identifier is static"

-- SCOPE UPDATING --
--------------------
decVar, decFunc :: UIdent -> ScopeM ()
decVar id = do
  checkRedeclaration id
  modify (newOccurence id)
decFunc id = do
  checkRedeclaration id
  modify (newOccurence id)
decType :: Type -> ScopeM ()
decType (TUser id) = do
  checkRedeclaration id
  modify (newOccurence id)
decType _ = return ()

-- SCOPE COMPUTATION --
-----------------------
varOrField id ifVar ifField = do
  field <- isField id
  case field of
    False -> resVar id >>= (return . ifVar)
    True -> scopeenvInstance `asCurrent` (resVar id >>= (return . ifField))

staticOrInstance id ifStatic ifInstance = do
  static <- isStatic id
  case static of
    False -> scopeenvStatic `asCurrent` (resFunc id >>= (return . ifStatic))
    True -> scopeenvInstance `asCurrent` (resFunc id >>= (return . ifInstance))

funH :: ClassHierarchy -> ScopeM ClassHierarchy
funH = Traversable.mapM $ \clazz@(Class typ super fields methods staticMethods loc) -> 
  Err.withLocation loc $ do
    typ' <- resType typ
    super' <- resType super
    enterClass clazz $ do
      (fields', methods') <-
        scopeenvFull `asCurrent` liftM2 (,) (mapM funF fields) (mapM funM methods)
      staticMethods' <- scopeenvStatic `asCurrent` mapM funMS staticMethods
      return $ Class typ' super' fields' methods' staticMethods' loc

funF :: Field -> ScopeM Field
funF (Field typ id origin) = liftM3 Field (resType typ) (resVar id) (resType origin)

funM :: Method -> ScopeM Method
funM (Method typ id ids stmt origin loc) =
  Err.withLocation loc $ do
    id' <- resFunc id
    typ' <- resType typ
    origin' <- resType origin
    newLocal $ do
      mapM_ decVar ids `rethrow` Err.duplicateArg id
      ids' <- mapM resVar ids
      stmt' <- newLocal (funS stmt)
      return $ Method typ' id' ids' stmt' origin' loc

funMS :: Method -> ScopeM Method
funMS = funM

funS :: Stmt -> ScopeM Stmt
funS x = case x of
  SDeclVar typ id -> do
    decVar id
    liftM2 SDeclVar (resType typ) (resVar id)
  SLocal _ stmts -> do -- definitions part of SLocal is empty at this point
    stmts' <- newLocal (mapM funS stmts)
    let decls = collectDeclarations stmts'
    let instrs = filterDeclarations stmts'
    return $ SLocal decls instrs
    where
      collectDeclarations :: [Stmt] -> [Variable]
      collectDeclarations = concatMap (\x -> case x of
          SDeclVar typ id -> return $ Variable typ id
          SMetaLocation _ stmts -> collectDeclarations stmts
          _ -> [])
      filterDeclarations :: [Stmt] -> [Stmt]
      filterDeclarations = concatMap (\x -> case x of
          SDeclVar typ id -> []
          SMetaLocation loc stmts -> return $ SMetaLocation loc $ filterDeclarations stmts
          _ -> return x)
  SAssign id expr -> do
    expr' <- funE expr
    -- Decide if this is actually a field access
    varOrField id (\id' -> SAssign id' expr') (\id' -> SAssignFld IThis id' expr')
  SAssignArr id expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    id' <- resVar id
    return $ SAssignArr id' expr1' expr2'
  SAssignFld id1 id2 expr2 -> do
    expr2' <- funE expr2
    id1' <- resVar id1
    scopeenvForeign id2 `asCurrent` do
      id2' <- resVar id2
      return $ SAssignFld id1' id2' expr2'
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
  STryCatch stmt1 typ2 id3 stmt4 -> do
    stmt1' <- newLocal (funS stmt1)
    newLocal $ do
      decVar id3
      id3' <- resVar id3
      -- catch body can hide exception variable
      stmt4' <- newLocal (funS stmt4)
      return $ STryCatch stmt1' typ2 id3' stmt4'
  SReturnV -> return SReturnV
  SEmpty -> return SEmpty
  SBuiltin -> return SBuiltin
  SInherited -> return SInherited
  SMetaLocation loc stmts -> stmtMetaLocation loc $ mapM funS stmts

funE :: Expr -> ScopeM Expr
funE x = case x of
  EVar id -> varOrField id (\id' -> EVar id') (\id' -> EAccessVar EThis id')
  EAccessArr expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    return $ EAccessArr expr1' expr2'
  ECall id exprs -> do
    exprs' <- mapM funE exprs
    staticOrInstance id (\id' -> ECall id' exprs') (\id' -> EAccessFn EThis id' exprs')
  EAccessFn EThis id exprs -> do
    exprs' <- mapM funE exprs
    scopeenvInstance `asCurrent` do
      id' <- resFunc id
      return $ EAccessFn EThis id' exprs'
  EAccessFn expr id exprs -> do
    expr' <- funE expr
    exprs' <- mapM funE exprs
    scopeenvForeign id `asCurrent` do
      id' <- resFunc id
      return $ EAccessFn expr' id' exprs'
  EAccessVar EThis id -> do
    scopeenvInstance `asCurrent` do
      id' <- resVar id
      return $ EAccessVar EThis id'
  EAccessVar expr id -> do
    expr' <- funE expr
    scopeenvForeign id `asCurrent` do
      id' <- resVar id
      return $ EAccessVar expr' id'
  ENewArr typ expr -> do
    typ' <- resType typ
    expr' <- funE expr
    return $ ENewArr typ' expr'
  ENewObj typ -> do
    typ' <- resType typ
    return $ ENewObj typ'
  EUnary _ op expr -> do
    expr' <- funE expr
    return $ EUnary TUnknown op expr'
  EBinary _ op expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    return $ EBinary TUnknown op expr1' expr2'
  _ -> return x


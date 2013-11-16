module Jvmm.Scope.Internal where

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
import Jvmm.Errors (rethrow, ErrorInfoT, runErrorInfoM)
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
type Symbols = Map.Map UIdent Tag
symbols0 = Map.empty

data Scope = Scope {
  scopeVars :: Symbols,
  scopeFuncs :: Symbols,
  scopeTypes :: Symbols
} deriving (Show)

scope0 = Scope symbols0 symbols0 symbols0

newOccurence :: Symbols -> UIdent -> Symbols
newOccurence m id = Map.insertWith (\_ -> (+ 1)) id tag0 m

resolve :: (Scope -> Symbols) -> UIdent -> Scope -> (ErrorInfoT Identity) UIdent
resolve acc id sc = case Map.lookup id (acc sc) of
  Just tag -> return $ tagWith tag id
  Nothing -> throwError $ Err.unboundSymbol id

data ScopeEnv = ScopeEnv {
    scopeenvInstance :: Scope
  , scopeenvStatic :: Scope
  , scopeenvParent :: Scope
} deriving (Show)

scopeenv0 = ScopeEnv scope0 scope0 scope0

-- We cannot resolve scope here without type information
scopeenvForeign :: UIdent -> ScopeEnv -> Scope
scopeenvForeign member _ = let scope1 = newOccurence symbols0 member in
  case member of
    FIdent _ -> scopeenv0 { scopeFuncs = scope1 }
    VIdent _ -> scopeenv0 { scopeVars = scope1 }
    _ -> error $ Err.unusedBranch "scopeenvForeign afked for member type"

scopeenvEmpty :: ScopeEnv -> Scope
scopeenvEmpty = const scope0

-- SCOPE MONAD --
-----------------
type ScopeM = StateT Scope (ReaderT ScopeEnv (ErrorInfoT Identity))
runScopeM :: ScopeM a -> ErrorInfoT Identity (a, Scope)
runScopeM m = runReaderT (runStateT m scope0) scopeenv0

resolveLocal, resolveInstance, resolveParent :: (Scope -> Symbols) -> UIdent -> ScopeM UIdent
resolveLocal acc id = (get) >>= (lift . lift . (resolve acc id))
resolveInstance acc id = (asks scopeenvInstance) >>= (lift . lift . (resolve acc id))
resolveParent acc id = (asks scopeenvParent) >>= (lift . lift . (resolve acc id))

checkRedeclaration:: (Scope -> Symbols) -> UIdent -> ScopeM ()
checkRedeclaration acc id = do
  -- resolve in block's beginning's scope
  idout <- resolveParent acc id `catchError` (\_ -> return id)
  -- resolve in current scope
  idin <- resolveLocal acc id `catchError` (\_ -> return id)
  unless (idin == idout) $ throwError $ Err.redeclaredSymbol id

-- SCOPE SWITCHING --
---------------------
-- Pushes scope 'stack frame', runs action in it, restores 'stack frame'
newLocal action = do
  parent <- get
  res <- local (\env -> env { scopeenvParent = parent }) action
  put parent
  return res

-- Sets current scope fromScopeEnv via given accessor
asCurrent query action = do
  saved <- get
  dest <- asks query
  put dest
  res <- action
  put saved
  return res

-- Returns current scope
getCurrent = get

-- SCOPE READING --
-------------------
resVar, resFunc :: UIdent -> ScopeM UIdent
resVar id = case id of
  IThis -> return id
  _ -> resolveLocal scopeVars id
resFunc = resolveLocal scopeFuncs
-- We do not support types hiding (but we want to check for redeclarations and usage of undeclared types)
resType :: Type -> ScopeM Type
resType typ = case typ of
  TUser id -> (resolveLocal scopeTypes id) >> (return typ)
  TFunc ret args excs -> liftM3 TFunc (resType ret) (mapM resType args) (mapM resType excs)
  _ -> return typ

isField :: UIdent -> ScopeM Bool
isField id = do
  -- resolve in global scope
  idout <- resolveInstance scopeVars id `catchError` (\_ -> return id)
  -- resolve in current scope
  idin <- resolveLocal scopeVars id `catchError` (\_ -> return id)
  return (idout == idin)

-- SCOPE UPDATING --
--------------------
decVar, decFunc :: UIdent -> ScopeM ()
decVar id = do
  checkRedeclaration scopeVars id
  modify $ (\sc -> sc { scopeVars = newOccurence (scopeVars sc) id })
decFunc id = do
  checkRedeclaration scopeFuncs id
  modify $ (\sc -> sc { scopeFuncs = newOccurence (scopeFuncs sc) id })
decType :: Type -> ScopeM ()
decType (TUser id) = do
  checkRedeclaration scopeTypes id
  modify $ (\sc -> sc { scopeTypes = newOccurence (scopeTypes sc) id })
decType _ = return ()

-- SCOPE COMPUTATION --
-----------------------
-- Fills current scope (the one in state) with mutually recursive declarations
-- Note that each class (user defined type) must reside within global
-- scope, since it forms a global scope itself, when resolving field
-- access, all we need to do is declare variable/function with given name
-- using global scope as a base, resolve this symbol and dispose modified
-- scope.
collectClasses :: ClassHierarchy -> ScopeM ()
collectClasses classes =
  Traversable.mapM (decType . classType) classes >> return ()

enterClassScope :: Class -> ScopeM a -> ScopeM a
enterClassScope clazz action = do
  globalScope <- 
  instanceScope <- scopenvEmpty `asCurrent` do
    mapM_ (\Field { fieldIdent = id } -> decVar id) $ classFields clazz
    mapM_ (\Method { methodIdent = id } -> decFunc id) $ classMethods clazz
  staticScope <- scopenvEmpty `asCurrent` do
    mapM_ (\Method { methodIdent = id } -> decFunc id) $ classStaticMethods clazz
  let parent = Scope {
      , scopeFuns = union (scopeFunc staticScope) (scopeFuns instanceScope)
      , scopeFuns = union (scopeFunc staticScope) (scopeFuns instanceScope)
      , scopeFuns = union (scopeFunc staticScope) (scopeFuns instanceScope)
  local (const $ ScopeEnv { scopeenvParent = parent }) action

funH :: ClassHierarchy -> ScopeM ClassHierarchy
funH = Traversable.mapM $ \clazz -> do
    typ' <- resType $ classType clazz
    super' <- resType $ classSuper clazz
    newLocal $ do
      enterClassScope clazz $ do
        fields' <- mapM funF $ classFields clazz
        methods' <- mapM funM $ classMethods clazz
        staticMethods' <- mapM funSM $ classStaticMethods clazz
        return $ Class {
              classType = typ'
            , classSuper = super'
            , classFields = fields'
            , classMethods = methods'
            , classStaticMethods = staticMethods'
          }

funF :: Field -> ScopeM Field
funF (Field typ id origin) = liftM3 Field (resType typ) (resVar id) (resType origin)

funM :: Method -> ScopeM Method
funM (Method typ id ids stmt origin) = do
  typ' <- resType typ
  id' <- resFunc id
  origin' <- resType origin
  newLocal $ do
    mapM_ decVar ids `rethrow` Err.duplicateArg typ id
    ids' <- mapM resVar ids
    stmt' <- newLocal (funS stmt)
    return $ Method typ' id' ids' stmt' origin'

funSM :: Method -> ScopeM Method
funSM = funM

funS :: Stmt -> ScopeM Stmt
funS x = case x of
  SDeclVar typ id -> do
    decVar id
    liftM2 SDeclVar (resType typ) (resVar id)
  SLocal _ stmts -> do -- definitions part of SLocal is empty at this point
    stmts' <- newLocal (mapM funS stmts)
    let (decls, instrs) = List.partition (\x -> case x of { SDeclVar _ _ -> True; _ -> False }) stmts'
    return $ SLocal (map (\(SDeclVar typ id) -> Variable typ id) decls) instrs
  SAssign id expr -> do
    expr' <- funE expr
    id' <- resVar id
    -- Decide if this is actually a field access
    field <- isField id
    return $ if field then SAssignFld IThis id' expr' else SAssign id' expr'
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

funE :: Expr -> ScopeM Expr
funE x = case x of
  EVar id -> do
    id' <- resVar id
    -- Decide if this is actually a field access
    field <- isField id
    return $ if field then EAccessVar EThis id' else EVar id'
  EAccessArr expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    return $ EAccessArr expr1' expr2'
  ECall id exprs -> do
    exprs' <- mapM funE exprs
    scopeenvStatic `asCurrent` do
      id' <- resFunc id
      return $ ECall id' exprs'
  EAccessFn expr id exprs -> do
    expr' <- funE expr
    exprs' <- mapM funE exprs
    scopeenvForeign id `asCurrent` do
      id' <- resFunc id
      return $ EAccessFn expr' id' exprs'
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


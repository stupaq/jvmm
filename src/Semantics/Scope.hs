module Semantics.Scope (scope, tagGlobal, tempIdent) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Traversable as Traversable

import Semantics.Commons
import qualified Semantics.Errors as Err
import Semantics.Errors (rethrow, ErrorInfoT, runErrorInfoM)
import Semantics.APTree

-- SCOPE REPRESENTATION --
--------------------------
type Tag = Int
tag0 = 0 :: Tag

tagWith :: Tag -> UIdent -> UIdent
tagWith tag = (+/+ concat ["$", show tag])

tagGlobal :: UIdent -> UIdent
tagGlobal = tagWith tag0

tempIdent :: UIdent -> String -> UIdent
tempIdent id ctx = id +/+ "#" +/+ ctx

-- Each symbol in scoped tree has an identifier which is unique in its scope,
-- in other words, there is no identifier hiding in scoped tree.
type Symbols = Map.Map UIdent Tag
symbols0 = Map.empty

data Scope = Scope {
  vars :: Symbols,
  funcs :: Symbols,
  types :: Symbols
} deriving (Eq, Show)
-- Default global scope
scope0 = Scope {
  vars = symbols0,
  funcs = symbols0,
  types = symbols0
}

newOccurence :: Symbols -> UIdent -> Symbols
newOccurence m id = Map.insertWith (\_ -> (+1)) id tag0 m

resolve :: (Scope -> Symbols) -> UIdent -> Scope -> (ErrorInfoT Identity) UIdent
resolve acc id sc = case Map.lookup id (acc sc) of
  Just tag -> return $ tagWith tag id
  Nothing -> throwError $ Err.unboundSymbol id

-- SCOPE MONAD --
-----------------
-- The first scope in reader monad is the global one, second is the one from
-- beginning of a block, scope in state is the current one, we need all of them
-- for tracking redeclarations in single block and resolving member symbols.
type ScopeM = StateT Scope (ReaderT (Scope, Scope) (ErrorInfoT Identity))
runScopeM :: ScopeM a -> ErrorInfoT Identity (a, Scope)
runScopeM m = runReaderT (runStateT m scope0) (scope0, scope0)

resolveLocal, resolveGlobal, resolveParent :: (Scope -> Symbols) -> UIdent -> ScopeM UIdent
resolveLocal acc id = (get) >>= (lift . lift . (resolve acc id))
resolveGlobal acc id = (asks fst) >>= (lift . lift . (resolve acc id))
resolveParent acc id = (asks snd) >>= (lift . lift . (resolve acc id))

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
  sc <- get
  (glob, _) <- ask
  res <- local (const (glob, sc)) action
  put sc
  return res

-- Runs action with global scope as current one, note that action will never
-- modify global scope inside, restores scope after action is executed
globalAsCurrent action = do
  sc <- get
  glob <- asks fst
  put glob
  res <- local (const (glob, glob)) action
  put sc
  return res

-- Runs action with current scope as global one, restores scope after action is
-- executed
currentAsGlobal action = do
  sc <- get
  res <- local (const (sc, sc)) action
  put sc
  return res

-- SCOPE READING --
-------------------
resVar, resFunc :: UIdent -> ScopeM UIdent
resVar = resolveLocal vars
resFunc = resolveLocal funcs
-- We do not support types hiding (but we want to check for redeclarations and usage of undeclared types)
resType :: Type -> ScopeM Type
resType typ = case typ of
  TUser id -> (resolveLocal types id) >> (return typ)
  TFunc ret args excs -> liftM3 TFunc (resType ret) (mapM resType args) (mapM resType excs)
  _ -> return typ

isField :: UIdent -> ScopeM Bool
isField id = do
  -- resolve in global scope
  idout <- resolveGlobal vars id `catchError` (\_ -> return id)
  -- resolve in current scope
  idin <- resolveLocal vars id `catchError` (\_ -> return id)
  return (idout == idin)

-- SCOPE UPDATING --
--------------------
decVar, decFunc :: UIdent -> ScopeM ()
decVar id = do
  checkRedeclaration vars id
  modify $ (\sc -> sc { vars = newOccurence (vars sc) id })
decFunc id = do
  checkRedeclaration funcs id
  modify $ (\sc -> sc { funcs = newOccurence (funcs sc) id })
decType :: Type -> ScopeM ()
decType (TUser id) = do
  checkRedeclaration types id
  modify $ (\sc -> sc { types = newOccurence (types sc) id })
decType _ = return ()

-- SCOPE COMPUTATION --
-----------------------
-- Creates scoped tree from translated AST
scope :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
scope classes = fmap fst $ runScopeM $ do
  collectClasses classes
  currentAsGlobal (funH classes)

-- Fills current scope (the one in state) with mutually recursive declarations
-- Note that each class (user defined type) must reside within global
-- scope, since it forms a global scope itself, when resolving field
-- access, all we need to do is declare variable/function with given name
-- using global scope as a base, resolve this symbol and dispose modified
-- scope.
collectClasses :: ClassHierarchy -> ScopeM ()
collectClasses classes =
  Traversable.mapM (decType . classType) classes >> return ()

collectMembers :: Class -> ScopeM ()
collectMembers clazz = do
  mapM_ (\(Field _ id) -> decVar id) $ classFields clazz
  mapM_ (\(Method _ id _ _) -> decFunc id) $ classMethods clazz

funH :: ClassHierarchy -> ScopeM ClassHierarchy
funH = Traversable.mapM $ \clazz -> do
    typ' <- resType $ classType clazz
    super' <- resType $ classSuper clazz
    newLocal $ do
      collectMembers clazz
      currentAsGlobal $ do
        fields' <- mapM funF $ classFields clazz
        methods' <- mapM funM $ classMethods clazz
        return $ clazz {
            classType = typ',
            classSuper = super',
            classFields = fields',
            classMethods = methods'
          }

funF :: Field -> ScopeM Field
funF (Field typ id) = liftM2 Field (resType typ) (resVar id)

funM :: Method -> ScopeM Method
funM (Method typ id ids stmt) = do
  typ' <- resType typ
  id' <- resFunc id
  newLocal $ do
    mapM_ decVar ids
    ids' <- mapM resVar ids `rethrow` Err.duplicateArg typ id
    stmt' <- newLocal (funS stmt)
    return $ Method typ' id' ids' stmt'

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
    return $ if field then SAssignFld EThis id' expr' else SAssign id' expr'
  SAssignArr id expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    id' <- resVar id
    return $ SAssignArr id' expr1' expr2'
  SAssignFld expr1 id2 expr2 -> do
    expr2' <- funE expr2
    expr1' <- funE expr1
    globalAsCurrent $ do
      -- We are in global scope now, member (depending on type, which we
      -- can't determine right now) may exist or not. We still can resolve
      -- member's name, since it will be declared immediately on top of
      -- global scope.
      decVar id2
      id2' <- resVar id2
      return $ SAssignFld expr1' id2' expr2'
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
  EAccessFn expr id exprs -> do
    expr' <- funE expr
    exprs' <- mapM funE exprs
    globalAsCurrent $ do
      -- We are in global scope now, member (depending on type, which we
      -- can't determine right now) may exist or not. We still can resolve
      -- member's name, since it will be declared immediately on top of
      -- global scope.
      decFunc id
      id' <- resFunc id
      return $ EAccessFn expr' id' exprs'
  EAccessVar expr id -> do
    expr' <- funE expr
    globalAsCurrent $ do
      -- We are in global scope now, member (depending on type, which we
      -- can't determine right now) may exist or not. We still can resolve
      -- member's name, since it will be declared immediately on top of
      -- global scope.
      decVar id
      id' <- resVar id
      return $ EAccessVar expr' id'
  EApp id exprs -> do
    id' <- resFunc id
    exprs' <- mapM funE exprs
    return $ EApp id' exprs'
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


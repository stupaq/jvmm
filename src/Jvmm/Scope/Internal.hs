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

-- SCOPE ENVIRONMENT --
-----------------------
data ScopeEnv = ScopeEnv {
    scopeenvInstance :: Scope
  , scopeenvStatic :: Scope
  , scopeenvParent :: Scope
} deriving (Show)

scopeenv0 = ScopeEnv scope0 scope0 scope0

-- We cannot lookupM scope here without type information
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

resolveInScope :: UIdent -> Scope -> ScopeM UIdent
resolveInScope id scope = case Map.lookup id scope of
  Just tag -> return $ tagWith tag id
  Nothing -> throwError $ Err.unboundSymbol id

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
    mapM_ (\Field { fieldIdent = id } -> declare id) $ classFields clazz
    mapM_ (\Method { methodIdent = id } -> declare id) $ classMethods clazz
    getCurrent
  staticScope <- const global `asCurrent` do
    mapM_ (\Method { methodIdent = id } -> declare id) $ classStaticMethods clazz
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
collectClasses classes = Traversable.mapM (declare' . classType) classes >> return ()

-- SCOPE QUERING --
-------------------
-- Two different identifiers
oneIdent, otherIdent :: UIdent
oneIdent = VIdent "#$#$#$#"
otherIdent = VIdent "@%@%@%@"

checkRedeclaration:: UIdent -> ScopeM ()
checkRedeclaration id = do
  idpar <- asCurrent scopeenvParent (resolve id) `catchError` (\_ -> return oneIdent)
  idloc <- resolve id `catchError` (\_ -> return oneIdent)
  unless (idpar == idloc) $ throwError $ Err.redeclaredSymbol id

isMember :: UIdent -> ScopeM Bool
isMember id = do
  idinst <- asCurrent scopeenvInstance (resolve id) `catchError` (\_ -> return oneIdent)
  idloc <- resolve id `catchError` (\_ -> return oneIdent)
  return (idinst == idloc)

isStatic :: UIdent -> ScopeM Bool
isStatic id = do
  idstat <- asCurrent scopeenvStatic (resolve id) `catchError` (\_ -> return oneIdent)
  idloc <- resolve id `catchError` (\_ -> return oneIdent)
  return (idstat == idloc)

resolve :: UIdent -> ScopeM UIdent
resolve id = case id of
  IThis -> return id
  _ -> get >>= resolveInScope id

resolve' :: Type -> ScopeM Type
resolve' typ = case typ of
  TUser id -> resolve id >> return typ
  TFunc ret args excs -> liftM3 TFunc (resolve' ret) (mapM resolve' args) (mapM resolve' excs)
  _ -> return typ

-- SCOPE UPDATING --
--------------------
declare :: UIdent -> ScopeM ()
declare id = checkRedeclaration id >> modify (newOccurence id)

declare' :: Type -> ScopeM ()
declare' (TUser id) = declare id
declare' _ = return ()

-- SCOPE COMPUTATION --
-----------------------
varOrField id ifVar ifField = do
  field <- isMember id
  case field of
    False -> resolve id >>= (return . ifVar)
    True -> scopeenvInstance `asCurrent` (resolve id >>= (return . ifField))

funH :: ClassHierarchy -> ScopeM ClassHierarchy
funH = Traversable.mapM $ \clazz@(Class typ super fields methods staticMethods loc) -> 
  Err.withLocation loc $ do
    typ' <- resolve' typ
    super' <- resolve' super
    enterClass clazz $ do
      (fields', methods') <-
        scopeenvFull `asCurrent` liftM2 (,) (mapM funF fields) (mapM funM methods)
      staticMethods' <- scopeenvStatic `asCurrent` mapM funMS staticMethods
      return $ Class typ' super' fields' methods' staticMethods' loc

funF :: Field -> ScopeM Field
funF (Field typ id origin) = liftM3 Field (resolve' typ) (resolve id) (resolve' origin)

funM :: Method -> ScopeM Method
funM (Method typ id ids stmt origin loc) =
  Err.withLocation loc $ do
    id' <- resolve id
    typ' <- resolve' typ
    origin' <- resolve' origin
    newLocal $ do
      mapM_ declare ids `rethrow` Err.duplicateArg id
      ids' <- mapM resolve ids
      stmt' <- newLocal (funS stmt)
      return $ Method typ' id' ids' stmt' origin' loc

funMS :: Method -> ScopeM Method
funMS = funM

funS :: Stmt -> ScopeM Stmt
funS x = case x of
  SDeclVar typ id -> do
    declare id
    liftM2 SDeclVar (resolve' typ) (resolve id)
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
  -- We replace all assignments that actually refer to instance fields but give precedence to local
  -- variables (like in Java), to referencee hidden field self.<field_name> construct can be used
  SAssign id expr -> do
    expr' <- funE expr
    varOrField id (\id' -> SAssign id' expr') (\id' -> SAssignFld IThis id' expr')
  SAssignArr id expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    id' <- resolve id
    return $ SAssignArr id' expr1' expr2'
  SAssignFld id1 id2 expr2 -> do
    expr2' <- funE expr2
    id1' <- resolve id1
    scopeenvForeign id2 `asCurrent` do
      id2' <- resolve id2
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
      declare id3
      id3' <- resolve id3
      -- Catch body can hide exception variable
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
  -- We replace all assignments that actually refer to instance method, we also give precedence to
  -- instance methods over static ones.
  ECall id exprs -> do
    exprs' <- mapM funE exprs
    member <- isMember id
    case member of
      True -> scopeenvInstance `asCurrent` do
        id' <- resolve id
        return $ EAccessFn EThis id' exprs'
      False -> scopeenvStatic `asCurrent` do
        id' <- resolve id
        return $ ECall id' exprs'
  EAccessFn EThis id exprs -> do
    exprs' <- mapM funE exprs
    scopeenvInstance `asCurrent` do
      id' <- resolve id
      return $ EAccessFn EThis id' exprs'
  EAccessFn expr id exprs -> do
    expr' <- funE expr
    exprs' <- mapM funE exprs
    scopeenvForeign id `asCurrent` do
      id' <- resolve id
      return $ EAccessFn expr' id' exprs'
  EAccessVar EThis id -> do
    scopeenvInstance `asCurrent` do
      id' <- resolve id
      return $ EAccessVar EThis id'
  EAccessVar expr id -> do
    expr' <- funE expr
    scopeenvForeign id `asCurrent` do
      id' <- resolve id
      return $ EAccessVar expr' id'
  ENewArr typ expr -> do
    typ' <- resolve' typ
    expr' <- funE expr
    return $ ENewArr typ' expr'
  ENewObj typ -> do
    typ' <- resolve' typ
    return $ ENewObj typ'
  EUnary _ op expr -> do
    expr' <- funE expr
    return $ EUnary TUnknown op expr'
  EBinary _ op expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    return $ EBinary TUnknown op expr1' expr2'
  _ -> return x


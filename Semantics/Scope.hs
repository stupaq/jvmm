module Semantics.Scope (scope, tagSymbol, tagSymbol', untagSymbol, tag0) where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Syntax.AbsJvmm (Ident(..), Type(..), Expr(..), Stmt(..))
import Semantics.Commons
import qualified Semantics.Errors as Err
import Semantics.Errors (rethrow)

-- TODO for scope resolution with classes we need to rewrite all EVar that
-- refer to object's scope to EAccessVar this (same with methods)

-- BUILTINS --
--------------
builtinGlobal = Map.fromList $ map (\name -> (Ident name, tag0)) [
    "printInt",
    "readInt",
    "printString",
    "readString",
    "error"]

-- SCOPE REPRESENTATION --
--------------------------
type Tag = Int
tag0 = 0 :: Tag

tagSymbol :: Int -> String -> Ident
tagSymbol tag = Ident . tagSymbol' tag

tagSymbol' :: Int -> String -> String
tagSymbol' tag name = concat [name, "$", show tag]

untagSymbol :: Ident -> String
untagSymbol (Ident id) = takeWhile (/= '$') id

-- Each symbol in scoped tree has an identifier which is unique in its scope,
-- in other words, there is no identifier hiding in scoped tree.
type Symbols = Map.Map Ident Tag
symbols0 = Map.empty

data Scope = Scope {
  vars :: Symbols,
  funcs :: Symbols,
  types :: Symbols
} deriving (Eq, Show)
scope0 = Scope {
  vars = symbols0,
  funcs = builtinGlobal,
  types = symbols0
}

newOccurence :: Symbols -> Ident -> Symbols
newOccurence m id = Map.insertWith (\_ -> (+1)) id tag0 m

resolve :: (Scope -> Symbols) -> Ident -> Scope -> (ErrorT String Identity) Ident
resolve acc id sc = case Map.lookup id (acc sc) of
  Just tag -> let (Ident name) = id in return $ Ident $ concat [name, "$", show tag]
  Nothing -> throwError $ Err.unboundSymbol id

-- SCOPE MONAD --
-----------------
-- The first scope in reader monad is the global one, second is the one from
-- beginning of a block, scope in state is the current one, we need all of them
-- for tracking redeclarations in single block and resolving member symbols.
type ScopeM = StateT Scope (ReaderT (Scope, Scope) (ErrorT String Identity))
runScopeM :: Scope -> ScopeM a -> Either String (a, Scope)
runScopeM sc m = runIdentity $ runErrorT $ runReaderT (runStateT m sc) (sc, sc)

resolveLocal, resolveGlobal, resolveParent :: (Scope -> Symbols) -> Ident -> ScopeM Ident
resolveLocal acc id = (get) >>= (lift . lift . (resolve acc id))
resolveGlobal acc id = (asks fst) >>= (lift . lift . (resolve acc id))
resolveParent acc id = (asks snd) >>= (lift . lift . (resolve acc id))

checkRedeclaration:: (Scope -> Symbols) -> Ident -> ScopeM ()
checkRedeclaration acc id = do
  -- resolve in block's beginning's scope
  idout <- resolveParent acc id `catchError` (\_ -> return id)
  -- resolve in current scope
  idin <- resolveLocal acc id `catchError` (\_ -> return id)
  unless (idin == idout) $ throwError $ Err.redeclaredSymbol id

-- SCOPE CHANGING --
--------------------
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

-- Fills current scope (the one in state) with mutually recursive declarations
-- Note that each class (user defined type) must reside within global
-- scope, since it forms a global scope itself, when resolving field
-- access, all we need to do is declare variable/function with given name
-- using global scope as a base, resolve this symbol and dispose modified
-- scope.
buildGlobal :: [Stmt] -> ScopeM ()
buildGlobal stmts = forM_ stmts $ \stmt -> case stmt of
  SDefFunc typ id args excepts stmt -> do
    decFunc id
  SDeclVar typ id -> do
    decVar id
  _ -> throwError $ Err.globalForbidden

resVar, resFunc :: Ident -> ScopeM Ident
resVar = resolveLocal vars
resFunc = resolveLocal funcs
-- We do not support types hiding (but we want to check for redeclarations and usage of undeclared types)
resType :: Type -> ScopeM Type
resType typ = case typ of
  TUser id -> (resolveLocal types id) >> (return typ)
  _ -> return typ

-- SCOPE RESOLUTION --
----------------------
decVar, decFunc, decType :: Ident -> ScopeM ()
decVar id = do
  checkRedeclaration vars id
  modify $ (\sc -> sc { vars = newOccurence (vars sc) id })
decFunc id = do
  checkRedeclaration funcs id
  modify $ (\sc -> sc { funcs = newOccurence (funcs sc) id })
decType id = do
  checkRedeclaration types id
  modify $ (\sc -> sc { types = newOccurence (types sc) id })

-- Creates scoped tree from translated AST
scope :: Stmt -> Either String Stmt
scope stmt = fmap fst $ runScopeM scope0 (funS stmt)

funND :: Stmt -> ScopeM Stmt
funND x = case x of
  -- We are in mutually recursive scope, these symbols are already defined
  SDefFunc typ id args excepts stmt -> do
    typ' <- resType typ
    id' <- resFunc id
    newLocal $ do
      args' <- mapM funA args `rethrow` (show x)
      stmt' <- newLocal (funS stmt)
      return $ SDefFunc typ' id' args' excepts stmt'
  SDeclVar typ id -> do
    typ' <- resType typ
    id' <- resVar id
    return $ SDeclVar typ' id'
  _ -> throwError $ Err.globalForbidden

funA :: Stmt -> ScopeM Stmt
funA (SDeclVar typ id) = do
  decVar id `rethrow` Err.duplicateArg typ id
  id' <- resVar id
  typ' <- resType typ
  return $ SDeclVar typ' id'

funS :: Stmt -> ScopeM Stmt
funS x = case x of
  Global stmts -> do
    buildGlobal stmts
    stmts' <- currentAsGlobal $ mapM funND stmts
    return $ Global stmts'
  SDefFunc typ id args excepts stmt -> do
    decFunc id
    funND $ SDefFunc typ id args excepts stmt
  SDeclVar typ id -> do
    decVar id
    funND $ SDeclVar typ id
  Local _ stmts -> do -- definitions part of Local is empty at this point
    stmts' <- newLocal (mapM funS stmts)
    let (decls, instrs) = List.partition (\x -> case x of { SDeclVar _ _ -> True; _ -> False }) stmts'
    return $ Local decls instrs
  SAssign id expr -> do
    expr' <- funE expr
    id' <- resVar id
    return $ SAssign id' expr'
  SAssignArr id expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    id' <- resVar id
    return $ SAssignArr id' expr1' expr2'
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
  Local _ _ -> undefined

funE :: Expr -> ScopeM Expr
funE x = case x of
  EVar id -> do
    id' <- resVar id
    return $ EVar id'
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
  EUnaryT _ op expr -> do
    expr' <- funE expr
    return $ EUnaryT TUnknown op expr'
  EBinaryT _ op expr1 expr2 -> do
    expr1' <- funE expr1
    expr2' <- funE expr2
    return $ EBinaryT TUnknown op expr1' expr2'
  _ -> return x


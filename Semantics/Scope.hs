{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Semantics.Scope where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Semantics.Errors as Err
import Semantics.Errors (rethrow)
import Syntax.AbsJvmm (Ident(..), Arg(..), Type(..), Expr(..), Stmt(..))

-- Each symbol in scoped tree has an identifier which is unique in its scope.
-- In other words, there is no identifier hiding in scoped tree
type Tag = Int
tag0 = 0 :: Tag
type Symbols = Map.Map Ident Tag
symbols0 = Map.empty

data Scope = Scope {
  vars :: Symbols,
  funcs :: Symbols,
  types :: Symbols
} deriving (Eq, Show)
--FIXME add built-ins to global scope
scope0 = Scope { vars = symbols0, funcs = symbols0, types = symbols0 }

declare :: Symbols -> Ident -> Symbols
declare m id = Map.insertWith (\_ -> (+1)) id tag0 m

resolve :: (Scope -> Symbols) -> Ident -> Scope -> (ErrorT String Identity) Ident
resolve acc id sc = case Map.lookup id (acc sc) of
  Just tag -> let (Ident name) = id in return $ Ident $ concat [name, "$", show tag]
  Nothing -> throwError $ Err.unboundSymbol id

type ScopeM = StateT Scope (ReaderT Scope (ErrorT String Identity))
runScopeM :: Scope -> ScopeM a -> Either String (a, Scope)
runScopeM sc m = runIdentity $ runErrorT $ runReaderT (runStateT m sc) sc

decVar, decFunc, decType :: Ident -> ScopeM ()
decVar id = do
  decl vars id
  modify $ (\sc -> sc { vars = declare (vars sc) id })
decFunc id = do
  decl funcs id
  modify $ (\sc -> sc { funcs = declare (funcs sc) id })
decType id = do
  decl types id
  modify $ (\sc -> sc { types = declare (types sc) id })

decl :: (Scope -> Symbols) -> Ident -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) ()
decl acc id = do
  -- resolve in block's beginning's scope
  idout <- res' acc id `catchError` (\_ -> return id)
  -- resolve in current scope
  idin <- res acc id `catchError` (\_ -> return id)
  unless (idin == idout) $ throwError $ Err.redeclaredSymbol id
  where
    res' :: (Scope -> Symbols) -> Ident -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Ident
    res' acc id = (ask) >>= (lift . lift . (resolve acc id))

resVar, resFunc :: Ident -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Ident
resVar = res vars
-- We do not support functions or types hiding (but we want to check for redeclarations and usage of undeclared types)
resFunc id = (res funcs id) >> (return id)
resType :: Type -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Type
resType typ = case typ of
  TUser id -> (res types id) >> (return typ)
  _ -> return typ

res :: (Scope -> Symbols) -> Ident -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Ident
res acc id = (get) >>= (lift . lift . (resolve acc id))

-- Pushes scope 'stack frame', runs action in it, restores 'stack frame'
local' action = do
  sc <- get
  res <- local (const sc) action
  put sc
  return res

-- Creates scoped tree from translated AST
scope :: Stmt -> Either String Stmt
scope stmt = fmap fst $ runScopeM scope0 (funS stmt)
  where
    -- The scope in reader monad is the one from beginning of a block, scope in
    -- state is the current one, we need both for tracking redeclarations in
    -- single block.
    -- Fills current scope (the one in state) with mutually recursive declarations
    buildGlobal :: [Stmt] -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) ()
    buildGlobal stmts = forM_ stmts $ \stmt -> case stmt of
      SDefFunc typ id args excepts stmt -> do
        decFunc id
      SDeclVar typ id -> do
        decVar id
      _ -> throwError $ Err.globalForbidden
    funND stmt = case stmt of
      -- We are in mutually recursive scope, these symbols are already defined
      SDefFunc typ id args excepts stmt -> do
        typ' <- resType typ
        id' <- resFunc id
        args' <- mapM funA args
        stmt' <- local' (funS stmt)
        return $ SDefFunc typ' id' args' excepts stmt'
      SDeclVar typ id -> do
        typ' <- resType typ
        id' <- resVar id
        return $ SDeclVar typ' id'
      _ -> throwError $ Err.globalForbidden
    funA :: Arg -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Arg
    funA (Arg typ id) = do
      decVar id `rethrow` Err.duplicateArg (Arg typ id)
      id' <- resVar id
      typ' <- resType typ
      return $ Arg typ' id'
    funS :: Stmt -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Stmt
    funS stmt = case stmt of
      Global stmts -> do
        buildGlobal stmts
        stmts' <- mapM funND stmts
        return $ Global stmts'
      SDefFunc typ id args excepts stmt -> do
        decFunc id
        funND $ SDefFunc typ id args excepts stmt
      SDeclVar typ id -> do
        decVar id
        funND $ SDeclVar typ id
      P1_SBlock stmts -> do
        stmts' <- local' (mapM funS stmts)
        return $ Local [] stmts' --FIXME
      SAssign id expr -> do
        expr' <- funE expr
        id' <- resVar id
        return $ SAssign id' expr'
      SAssignOp id opassign expr -> do
        expr' <- funE expr
        id' <- resVar id
        return $ SAssignOp id' opassign expr'
      SAssignArr id expr1 expr2 -> do
        expr1' <- funE expr1
        expr2' <- funE expr2
        id' <- resVar id
        return $ SAssignArr id' expr1' expr2'
      SPostInc id -> do
        id' <- resVar id
        return $ SPostInc id'
      SPostDec id -> do
        id' <- resVar id
        return $ SPostDec id'
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
      SForeach typ id expr stmt -> do
        typ' <- resType typ
        expr' <- funE expr
        local' $ do
          decVar id
          id' <- resVar id
          -- function body can hide iteration variable
          stmt' <- local' (funS stmt)
          return $ SForeach typ' id' expr' stmt'
      SExpr expr -> do
        expr' <- funE expr
        return $ SExpr expr'
      SThrow expr -> do
        expr' <- funE expr
        return $ SThrow expr'
      STryCatch stmt1 typ2 id3 stmt4 -> do
        stmt1' <- local' (funS stmt1)
        local' $ do
          decVar id3
          id3' <- resVar id3
          -- catch body can hide exception variable
          stmt4' <- local' (funS stmt4)
          return $ STryCatch stmt1' typ2 id3' stmt4'
      SReturnV -> return SReturnV
      SEmpty -> return SEmpty
      Local _ _ -> undefined
    funE :: Expr -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Expr
    funE expr = case expr of
      EVar id -> do
        id' <- resVar id
        return $ EVar id'
      EAccessArr expr1 expr2 -> do
        expr1' <- funE expr1
        expr2' <- funE expr2
        return $ EAccessArr expr1' expr2'
      EAccessFn expr id exprs -> do
        let id' = id -- NOTE field access
        expr' <- funE expr
        exprs' <- mapM funE exprs
        return $ EAccessFn expr' id' exprs'
      EAccessVar expr id -> do
        expr' <- funE expr
        let id' = id -- NOTE method call
        return $ EAccessVar expr' id'
      EApp id exprs -> do
        id' <- resFunc id
        exprs' <- mapM funE exprs
        return $ EApp id' exprs'
      ENewArr typ expr -> do
        typ' <- resType typ
        expr' <- funE expr
        return $ ENewArr typ' expr'
      ENeg expr -> do
        expr' <- funE expr
        return $ ENeg expr'
      ENot expr -> do
        expr' <- funE expr
        return $ ENot expr'
      EMul expr1 opmul2 expr3 -> do
        expr1' <- funE expr1
        expr3' <- funE expr3
        return $ EMul expr1' opmul2 expr3'
      EAdd expr1 opadd2 expr3 -> do
        expr1' <- funE expr1
        expr3' <- funE expr3
        return $ EAdd expr1' opadd2 expr3'
      ERel expr1 oprel2 expr3 -> do
        expr1' <- funE expr1
        expr3' <- funE expr3
        return $ ERel expr1' oprel2 expr3'
      EAnd expr1 expr2 -> do
        expr1' <- funE expr1
        expr2' <- funE expr2
        return $ EAnd expr1' expr2'
      EOr expr1 expr2 -> do
        expr1' <- funE expr1
        expr2' <- funE expr2
        return $ EOr expr1' expr2'
      _ -> return expr


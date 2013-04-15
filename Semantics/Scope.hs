module Semantics.Scope where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Either as Either
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
-- We do not support functions or types hiding (yet)
  funcs :: Symbols,
  types :: Symbols
} deriving (Eq, Show)

--FIXME add built-ins to global scope
scope0 = Scope { vars = symbols0, funcs = symbols0, types = symbols0 }

declare :: Symbols -> Ident -> Symbols
declare m id = Map.insertWith (\_ -> (+1)) id tag0 m

decVar, decFunc, decType :: Ident -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) ()
decVar id = do
  redecl vars id `rethrow` Err.redeclaredSymbol id
  modify $ (\sc -> sc { vars = declare (vars sc) id })
decFunc id = do
  redecl vars id `rethrow` Err.redeclaredSymbol id
  modify $ (\sc -> sc { funcs = declare (funcs sc) id })
decType id = do
  redecl vars id `rethrow` Err.redeclaredSymbol id
  modify $ (\sc -> sc { types = declare (types sc) id })

redecl :: (Scope -> Symbols) -> Ident -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) ()
redecl acc id = do
  -- resolve in block's beginning's scope
  idout <- res' acc id `catchError` (\_ -> return id)
  -- resolve in current scope
  idin <- res acc id `catchError` (\_ -> return id)
  unless (idin == idout) $ throwError $ Err.redeclaredSymbol id
  where
    res' :: (Scope -> Symbols) -> Ident -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Ident
    res' acc id = (ask) >>= (lift . lift . (resolve acc id))

res :: (Scope -> Symbols) -> Ident -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Ident
res acc id = (get) >>= (lift . lift . (resolve acc id))

resolve :: (Scope -> Symbols) -> Ident -> Scope -> (ErrorT String Identity) Ident
resolve acc id sc = case Map.lookup id (acc sc) of
  Just tag -> let (Ident name) = id in return $ Ident $ concat [name, "$", show tag]
  Nothing -> throwError $ Err.unboundSymbol id

-- Pushes scope 'stack frame', runs action in it, restores 'stack frame'
local' action = do
  sc <- get
  res <- local (const sc) action
  put sc
  return res

-- Creates scoped tree from translated AST
scope :: Stmt -> Either String Stmt
scope stmt = runIdentity $ runErrorT $ runReaderT (evalStateT (funS stmt) scope0) scope0
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
      SDefVar typ id expr -> do
        decVar id
      _ -> throwError $ Err.globalForbidden
    funND stmt = case stmt of
      -- We are in mutually recursive scope, these symbols are already defined
      SDefFunc typ id args excepts stmt -> do
        id' <- res funcs id
        args' <- mapM funA args
        stmt' <- local' (funS stmt)
        return $ SDefFunc typ id' args' excepts stmt'
      SDeclVar typ id -> do
        id' <- res vars id
        return $ SDeclVar typ id'
      SDefVar typ id expr -> do
        expr' <- funE expr
        id' <- res vars id
        return $ SDefVar typ id' expr'
      _ -> throwError $ Err.globalForbidden
    funA :: Arg -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Arg
    funA (Arg typ id) = do
      decVar id `rethrow` Err.duplicateArg (Arg typ id)
      id' <- res vars id
      return $ Arg typ id'
    funS :: Stmt -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Stmt
    funS stmt = case stmt of
      Global stmts -> do
        buildGlobal stmts
        stmts' <- mapM funND stmts
        return $ SBlock stmts'
      SDefFunc typ id args excepts stmt -> do
        decFunc id
        funND $ SDefFunc typ id args excepts stmt
      SDeclVar typ id -> do
        decVar id
        funND $ SDeclVar typ id
      SDefVar typ id expr -> do
        decVar id
        funND $ SDefVar typ id expr
      SBlock stmts -> do
        stmts' <- local' (mapM funS stmts)
        return $ SBlock stmts'
      SAssign id expr -> do
        expr' <- funE expr
        id' <- res vars id
        return $ SAssign id' expr'
      SAssignOp id opassign expr -> do
        expr' <- funE expr
        id' <- res vars id
        return $ SAssignOp id' opassign expr'
      SAssignArr id expr1 expr2 -> do
        expr1' <- funE expr1
        expr2' <- funE expr2
        id' <- res vars id
        return $ SAssignArr id' expr1' expr2'
      SPostInc id -> do
        id' <- res vars id
        return $ SPostInc id'
      SPostDec id -> do
        id' <- res vars id
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
        expr' <- funE expr
        local' $ do
          decVar id
          id' <- res vars id
          -- function body can hide iteration variable
          stmt' <- local' (funS stmt)
          return $ SForeach typ id' expr' stmt'
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
          id3' <- res vars id3
          -- catch body can hide exception variable
          stmt4' <- local' (funS stmt4)
          return $ STryCatch stmt1' typ2 id3' stmt4'
      _ -> undefined
    funE :: Expr -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Expr
    funE expr = case expr of
      EVar id -> do
        id' <- res vars id
        return $ EVar id'
      EAccessArr expr1 expr2 -> do
        expr1' <- funE expr1
        expr2' <- funE expr2
        return $ EAccessArr expr1' expr2'
      EAccessFn expr id exprs -> do
        let id' = id --TODO fields
        expr' <- funE expr
        exprs' <- mapM funE exprs
        return $ EAccessFn expr' id' exprs'
      EAccessVar expr id -> do
        expr' <- funE expr
        let id' = id --TODO methods
        return $ EAccessVar expr' id'
      EApp id exprs -> do
        id' <- res funcs id
        exprs' <- mapM funE exprs
        return $ EApp id' exprs'
      ENewArr typ expr -> do
        expr' <- funE expr
        return $ ENewArr typ expr'
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


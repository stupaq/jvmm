{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
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

scope0 = Scope { vars = symbols0, funcs = symbols0, types = symbols0 }

declare :: Symbols -> Ident -> Symbols
declare m id = Map.insertWith (\_ -> (+1)) id tag0 m

decVar, decFunc, decType :: Ident -> Scope -> Scope
decVar id sc = sc { vars = declare (vars sc) id }
decFunc id sc = sc { funcs = declare (funcs sc) id }
decType id sc = sc { types = declare (types sc) id }

--FIXME add state to funS and therefore return side effects of a statement in parent block
-- How each statement affects current scope
declareStmt :: Stmt -> Scope -> Scope
declareStmt stmt = case stmt of
  SDefFunc _ id _ _ -> decFunc id
  SDeclVar typ id -> decVar id
  SDefVar typ id _ -> decVar id
  _ -> Prelude.id

--TODO fake declaration and resolution for functions and types

res' :: (Scope -> Symbols) -> Ident -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Ident
res' acc id = (get) >>= (lift . lift . (resolve acc id))

res :: (Scope -> Symbols) -> Ident -> (ReaderT Scope (ErrorT String Identity)) Ident
res acc id = (ask) >>= (lift . (resolve acc id))

resolve :: (Scope -> Symbols) -> Ident -> Scope -> (ErrorT String Identity) Ident
resolve acc id sc = case Map.lookup id (acc sc) of
  Just tag -> let (Ident name) = id in return $ Ident $ concat [name, "$", show tag]
  Nothing -> throwError $ Err.unboundSymbol id

-- If we need no state on error, we stack monads this way
evalErrorState m = runIdentity . runErrorT . (evalStateT m)
runErrorState m = runIdentity . runErrorT . (runStateT m)

runErrorEnv m = runIdentity . runErrorT . (runReaderT m)

-- Creates scoped tree from translated AST, sequential symbol declarations
scope' :: Stmt -> Either String Stmt
scope' stmt = runErrorEnv (funS stmt) scope0
  where
    -- The scope in reader monad is the one from outside of a function,
    -- scope in state is the 'current' one -- for tracking redeclarations
    funA :: Arg -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Arg
    funA (Arg typ id) = do
      -- resolve in reader's Scope
      idout <- lift $ res vars id `catchError` (\_ -> return id)
      -- resolve in state's scope
      idin <- res' vars id `catchError` (\_ -> return id)
      unless (idin == idout) $ throwError $ Err.duplicateArg (Arg typ id)
      modify (decVar id)
      -- resolve in state's scope
      id' <- res' vars id
      return $ Arg typ id'
    funS :: Stmt -> (ReaderT Scope (ErrorT String Identity)) Stmt
    funS stmt = case stmt of
      SDefFunc typ id args stmt -> do
        sc <- ask
        (args', sc') <- runStateT (mapM funA args) sc
        stmt' <- local (const sc') (funS stmt)
        return $ SDefFunc typ id args' stmt'
      SDeclVar typ id -> do
        id' <- res vars id
        return $ SDeclVar typ id'
      SDefVar typ id expr -> do
        expr' <- funE expr
        id' <- res vars id
        return $ SDefVar typ id' expr'
      SBlock stmts -> do --FIXME forbid redeclaration in the same scope
        sc <- ask
        stmts' <- evalStateT (mapM fun stmts) sc
        return $ SBlock stmts'
        where
          -- The scope in reader monad is the one from outer block,
          -- scope in state is the 'current' one -- for tracking redeclarations
          fun :: Stmt -> (StateT Scope (ReaderT Scope (ErrorT String Identity))) Stmt
          fun stmt = do
            modify (declareStmt stmt)
            sc' <- get
            case runErrorEnv (funS stmt) sc' of
              Right stmt' -> return stmt'
              Left err -> throwError err
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
        local (decVar id) $ do
          id' <- (res vars id)
          stmt' <- funS stmt
          return $ SForeach typ id' expr' stmt'
      SExpr expr -> do
        expr' <- funE expr
        return $ SExpr expr'
      _ -> return stmt
    funE :: Expr -> (ReaderT Scope (ErrorT String Identity)) Expr
    funE expr = case expr of
      EVar id -> do
        id' <- res vars id
        return $ EVar id'
      EAccessArr expr1 expr2 -> do
        expr1' <- funE expr1
        expr2' <- funE expr2
        return $ EAccessArr expr1' expr2'
      EAccessFn expr id exprs -> do
        let id' = id
        expr' <- funE expr
        exprs' <- mapM funE exprs
        return $ EAccessFn expr' id' exprs'
      EAccessVar expr id -> do
        expr' <- funE expr
        let id' = id
        return $ EAccessVar expr' id'
      EApp id exprs -> do
        exprs' <- mapM funE exprs
        return $ EApp id exprs'
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

-- TODO
{-
-- Creates scoped tree from translated AST, mutually recursive symbols,
-- allows only blocks of function definitions
scope'' :: Stmt -> Either String Stmt
scope'' (SBlock stmts) = runErrorEnv (funS stmt) scope0
  sc <- ask
  sc' <- foldM fun sc stmts
  return $ Block $ map (fun' sc) stmts
  where
-- Aggregates declarations in global scope
    fun sc (SDefFunc typ id args blk) = return $ sc { funcs = declare (funcs sc) id }
    fun _ (SDeclVars typ ids) = Err.globalVarDec ids
    fun _ _ = Err.globalNonDec
-- Recurses into declaration's blocks
    fun' sc (SDefFunc typ id args blk) = undefined
-- -}

-- Scope computation always starts with global scope
scope :: Stmt -> Either String Stmt
scope = scope' --FIXME

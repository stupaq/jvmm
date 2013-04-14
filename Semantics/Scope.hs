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

data Scope = Scope {
  vars :: Map.Map Ident Tag,
-- We do not support functions or types hiding (yet)
  funcs :: Map.Map Ident Tag,
  types :: Map.Map Ident Tag
} deriving (Eq, Show)

scope0 = Scope { vars = Map.empty, funcs = Map.empty, types = Map.empty }

declare :: Map.Map Ident Tag -> Ident -> Map.Map Ident Tag
declare m id = Map.insertWith (\_ -> (+1)) id tag0 m

decVar, decFunc, decType :: Ident -> Scope -> Scope
decVar id sc = sc { vars = declare (vars sc) id }
decFunc id sc = sc { funcs = declare (funcs sc) id }
decType id sc = sc { types = declare (types sc) id }

-- How each statement affects current scope
declareStmt :: Stmt -> Scope -> Either String Scope
declareStmt stmt sc = case stmt of
  SDefFunc _ id _ _ -> return $ decFunc id sc
  SDeclVar typ id -> return $ decVar id sc
  SDefVar typ id _ -> return $ decVar id sc
  _ -> return sc

-- If we need no state on error
runErrorState m = runIdentity . runErrorT . (evalStateT m)

-- How arguments affect scope inside function body
declareArgs :: [Arg] -> Scope -> Either String Scope
declareArgs args sc = runErrorState (foldM fun sc args) Set.empty
  where
    fun :: Scope -> Arg -> (StateT (Set.Set Ident) (ErrorT String Identity)) Scope
    fun sc (Arg typ id) = do
      args <- gets (Set.member id)
      modify (Set.insert id)
      case args of
        False -> return $ decVar id sc
        True -> throwError $ Err.duplicateArg (Arg typ id)

resolve :: Map.Map Ident Tag -> Ident -> (ReaderT Scope (ErrorT String Identity)) Ident
resolve m id = case Map.lookup id m of
    Just tag -> return $ Ident $ concat [show id, show tag]
    Nothing -> throwError $ Err.unboundSymbol id

resVar, resFunc, resType :: Ident -> (ReaderT Scope (ErrorT String Identity)) Ident
resVar id = (ask) >>= (\sc -> resolve (vars sc) id)
resFunc id = (ask) >>= (\sc -> resolve (funcs sc) id)
resType id = (ask) >>= (\sc -> resolve (types sc) id)

runErrorEnv m = runIdentity . runErrorT . (runReaderT m)

-- Creates scoped tree from translated AST, sequential symbol declarations
scope' :: Stmt -> Either String Stmt
scope' stmt = runErrorEnv (funS stmt) scope0
  where
    funS :: Stmt -> (ReaderT Scope (ErrorT String Identity)) Stmt
    funS stmt = case stmt of
      SDefFunc typ id args stmt -> do
        sc <- ask
        case declareArgs args sc of
          Right sc' -> do
            let args' = args --FIXME args are not relabelled
            stmt' <- local (const sc') (funS stmt)
            return $ SDefFunc typ id args' stmt'
          Left err -> throwError err
      SDeclVar typ id -> do
        id' <- resVar id
        return $ SDeclVar typ id'
      SDefVar typ id expr -> do
        expr' <- funE expr
        id' <- resVar id
        return $ SDefVar typ id' expr'
      SBlock stmts -> undefined --TODO
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
        id' <- resVar id
        expr' <- funE expr
        stmt' <- local (decVar id) (funS stmt)
        return $ SForeach typ id' expr' stmt'
      SExpr expr -> do
        expr' <- funE expr
        return $ SExpr expr'
      _ -> return stmt
    funE :: Expr -> (ReaderT Scope (ErrorT String Identity)) Expr
    funE = undefined

-- TODO
{-
-- Creates scoped tree from translated AST, mutually recursive symbols,
-- allows only blocks of function definitions
scope'' :: Block -> ScopeM Block
scope'' (Block stmts) = do
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

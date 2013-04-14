{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Semantics.Scope where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Semantics.Errors as Err
import Syntax.AbsJvmm as Abs

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

resolve :: Map.Map Ident Tag -> Ident -> Either String Ident
resolve m id = case Map.lookup id m of
    Just tag -> return $ Ident $ concat [show id, show tag]
    Nothing -> Err.unboundVar id

declare :: Map.Map Ident Tag -> Ident -> Map.Map Ident Tag
declare m id = Map.insertWith (\_ -> (+1)) id tag0 m

decVar, decFunc, decType :: Scope -> Ident -> Scope
decVar sc id = sc { vars = declare (vars sc) id }
decFunc sc id = sc { funcs = declare (funcs sc) id }
decType sc id = sc { types = declare (types sc) id }

-- How each statement affects current scope
declareStmt :: Stmt -> Scope -> Either String Scope
declareStmt stmt sc = case stmt of
  SDefFunc _ id _ _ -> return $ decFunc sc id
  SDeclVars typ items -> return $ foldl (\sc it -> case it of
    NoInit id  -> decVar sc id
    Init id _  -> decVar sc id) sc items
  _ -> return sc

-- If we need no state
runErrorState :: (Error a') => StateT a (ErrorT a' Identity) a'' -> a -> Either a' a''
runErrorState m = runIdentity . runErrorT . (evalStateT m)

-- How arguments affect scope inside function body
declareArgs :: [Arg] -> Scope -> Either String Scope
declareArgs args sc =
  runErrorState (foldM fun sc args) Set.empty
  where
    fun :: Scope -> Arg -> (StateT (Set.Set Ident) (ErrorT String Identity)) Scope
    fun sc (Arg typ id) = do
      args <- gets (Set.member id)
      modify (Set.insert id)
      case args of
        False -> return $ decVar sc id
        True -> Err.duplicateArg (Arg typ id)

-- Monad for assigning scope to variables
type ScopeM a = (ReaderT Scope (ErrorT String Identity)) a
runScopeM :: Scope -> ScopeM a -> Either String a
runScopeM sc m = runIdentity (runErrorT (runReaderT m sc))

-- TODO
-- Creates scoped tree from AST for SBlock, sequential symbol declarations
scopeS :: Stmt -> ScopeM Stmt
scopeS (Block stmts) = do
  sc <- ask
  fail "undefined"
  where
    scope'' :: Stmt -> (ReaderT Scope (ErrorT String Identity)) [Stmt]
    scope'' stmt =
      case stmt of
      -- TODO
        SDefFunc typ id args block -> undefined
        SDeclVars typ items -> undefined
        SForeach typ id expr stmt -> do
          stmt' <- pack $ scope'' stmt
          pass $ S expr stmt'
        SStmtBlock (Block stmts) -> do
          stmt' <- pack $ scope'' stmt
          pass $ SBlock stmt'
        SIf expr stmt -> do
          stmt' <- pack $ scope'' stmt
          pass $ SIf expr stmt'
        SIfElse expr stmt1 stmt2 -> do
          stmt1' <- pack $ scope'' stmt1
          stmt2' <- pack $ scope'' stmt2
          pass $ SIfElse expr stmt1' stmt2'
        SWhile expr stmt -> do
          stmt' <- pack $ scope'' stmt
          pass $ SWhile expr stmt'
        _ -> pass stmt
      where
        pack :: [Stmt] -> Stmt
        pack [stmt] = stmt
        pack stmts = SBlock $ Block

{-
-- Creates scoped tree from AST for SBlock, mutually recursive symbols,
-- allows only blocks of function definitions
scopeR :: Block -> ScopeM Block
scopeR (Block stmts) = do
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
scopeR = undefined

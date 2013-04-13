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
import Syntax.AbsJvmm

-- Each symbol in scoped tree has an identifier which is unique in its scope.
-- In other words, there is no identifier hiding in Scope.Tree
type Tree = Block
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

-- How arguments affect scope inside function body
declareArgs :: [Arg] -> Scope -> Either String Scope
declareArgs args sc =
  runIdentity $ runErrorT $ evalStateT (foldM fun sc args) Set.empty
  where
    fun :: Scope -> Arg -> (StateT (Set.Set Ident) (ErrorT String Identity)) Scope
    fun sc (Arg typ id) = do
      args <- gets (Set.member id)
      modify (Set.insert id)
      case args of
        False -> return $ decVar sc id
        True -> Err.duplicateArg (Arg typ id)

-- Monad for assigning scope to variables
type ScopeM a = (ReaderT Scope (ErrorT String (Identity))) a
runScopeM :: Scope -> ScopeM a -> Either String a
runScopeM sc m = runIdentity (runErrorT (runReaderT m sc))

-- Creates Scope.Tree from AST for Prog
scope :: Prog -> ScopeM Tree
scope (Prog defs) = scopeR $ Block $ map fun defs
  where fun (GDefFunc typ id args blk) = SDefFunc typ id args blk

-- TODO
-- Creates Scope.Tree from AST for Block, sequential symbol declarations
scopeS :: Block -> ScopeM Tree
scopeS (Block []) = undefined

-- Creates Scope.Tree from AST for Block, mutually recursive symbols,
-- allows only blocks of function definitions
scopeR :: Block -> ScopeM Tree
scopeR (Block stmts) = do
  sc <- ask
  sc' <- foldM fun sc stmts
  return $ Block $ map (fun' sc) stmts
  where
-- Aggregates declarations in global scope
    fun sc (SDefFunc typ id args blk) = return $ sc { funcs = declare (funcs sc) id }
    fun _ (SDeclVars typ ids) = throwError $ "variable declaration in global scope: " ++ (concat $ map show ids)
    fun _ _ = throwError $ "not a declaration in global scope"
-- Recurses into declaration's blocks
    fun' sc (SDefFunc typ id args blk) = undefined


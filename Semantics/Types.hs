{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Semantics.Types where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Semantics.Scope as Scope
import qualified Semantics.Errors as Err
import Semantics.Errors (rethrow)
import Syntax.AbsJvmm (Ident(..), Arg(..), Type(..), Expr(..), Stmt(..))

-- Typing is fully static. We type each null as Object type (which is a
-- superclass of every non-primitive value).

-- Adds exception throwed by statement
throws :: Type -> ReaderT TypeEnv (ErrorT String (WriterT (Set Type) Identity)) ()
throws = tell . Set.singleton

type Types = Map.Map Ident Type
types0 = Map.empty

type MemberTypes = Map.Map Type Types
membertypes0 = Map.empty

data TypeEnv = TypeEnv {
  vars :: Types,
  funcs :: Types,
--TODO handle composite types
  types :: MemberTypes
}
typeenv0 = TypeEnv { vars = types0, funcs = types0, types = membertypes0 }

typeof :: (TypeEnv -> Types) -> Ident -> TypeEnv -> (ErrorT String Identity) Type
typeof acc id env = case Map.lookup (acc env) of
  Just typ -> return typ
  Nothing -> throwError $ Err.unknownType id

type TypeM = ReaderT TypeEnv (WriterT (Set.Set Type) (ErrorT String Identity))
runTypeM :: TypeEnv -> (Set.Set Type) -> TypeM a -> Either String (a, Set.Set Type)
runTypeM r w = runIdentity . runErrorT . (runWriterT w) . (runReaderT r)

typeofFunc, typeofVar :: Ident -> TypeM Type
typeofVar id = (ask) >>= (lift . lift . (typeof vars id))
typeofFunc id = (ask) >>= (lift . lift . (typeof funcs id))

typeofMem :: Type -> Ident -> TypeM Type
typeofMem typ id = (asks types) >>= (lift . lift . (typeof )

typeVar :: Ident -> Type -> TypeM ()
typeVar id typ =
  

types :: Stmt -> Either String (Type, Set.Set Type)
types = runTypeM
  where
    buildGlobal :: [Stmt] -> TypeM TypeEnv
    buildGlobal stmts = lift $ lift $ runStateT Scope.scope0 (mapM_ funS stmts)
      where
        funS :: Stmt -> (StateT TypeEnv (ErrorT String Identity)) ()
        funS stmt = case stmt of
          SDefFunc typ id args excepts stmt -> do
            decFunc id
          SDeclVar typ id -> do
            decVar id
          SDefVar typ id expr -> do
            decVar id
          _ -> return ()
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
      SDefVar typ id expr -> do
        typ' <- resType typ
        expr' <- funE expr
        id' <- resVar id
        return $ SDefVar typ' id' expr'
      _ -> throwError $ Err.globalForbidden
    funS :: Stmt -> TypeM Type
    funS stmt = case stmt of
      Global stmts -> do
        env <- buildGlobal stmts
        mapM funND stmts
        return TVoid
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
      SReturnV -> return SReturnV
      SEmpty -> return SEmpty
    funE :: Expr -> TypeM Type
    funE expr = case expr of
      EVar id -> do
        id' <- res vars id
        return $ EVar id'
      ELitInt n -> return TInt
      ELitTrue -> return TBool
      ELitFalse -> return TBool
      ELitString str -> return TString
      ELitChar c -> return TChar
      ENull -> return TObject
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


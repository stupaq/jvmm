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
import Semantics.Trans (UIdent(..), toStr)
import Syntax.AbsJvmm (Arg(..), Type(..), Expr(..), Stmt(..), OpRel(..), OpAdd(Plus), OpAssign(APlus))
import Syntax.AbsJvmm (Ident)

-- Typing is fully static. We type each null as Object type (which is a
-- superclass of every non-primitive value).
-- Note that after scope resolution we can't throw undeclared errors (also for
-- user defined types)

-- Typing data
type Types = Map.Map UIdent Type
types0 = Map.empty

type MemberTypes = Map.Map Type Types
membertypes0 = Map.empty

data TypeEnv = TypeEnv {
  idents :: Types,
--TODO handle composite types
  types :: MemberTypes
}
typeenv0 = TypeEnv { idents = types0, types = membertypes0 }

-- Domain-specific dialect
type TypeM = ReaderT TypeEnv (WriterT (Set.Set Type) (ErrorT String Identity))
runTypeM :: TypeEnv -> TypeM a -> Either String (a, Set.Set Type)
runTypeM r m = runIdentity $ runErrorT $ runWriterT $ runReaderT m r

lookupM :: (Ord a) => a -> Map.Map a b -> TypeM b
lookupM key map = lift $ lift $ case Map.lookup key map of
  Just val -> return val
  Nothing -> throwError noMsg

typeof :: UIdent -> TypeM Type
typeof uid = asks idents >>= lookupM uid 

typeof' :: Type -> UIdent -> TypeM Type
typeof' typ uid = (asks types >>= lookupM typ >>= lookupM uid) `rethrow` Err.unknownMemberType typ uid

throws :: Type -> TypeM ()
throws = tell . Set.singleton

declare :: UIdent -> Type -> TypeM ()
declare uid typ = undefined--FIXME

(=||=) :: TypeM Type -> TypeM Type -> TypeM Type
(=||=) m1 m2 = do
  typ1 <- m1
  typ2 <- m2
  unless (typ1 == typ2) $ throwError $ Err.unexpectedType typ1 typ2
  return typ1

(-||-) :: TypeM Type -> TypeM Type -> TypeM Type
(-||-) m1 m2 = (m1 =||= m2) >> (return TVoid)

(=|) :: Type -> TypeM Type -> TypeM Type
(=|) typ1 = (=||=) (return typ1)

(-|) :: Type -> TypeM Type -> TypeM Type
(-|) typ1 m2 = (typ1 =| m2) >> (return TVoid)

infix 3 `instead`
instead :: Bool -> TypeM a -> TypeM ()
instead cond m = unless cond $ m >> return ()

--TODO remove these when ready
typeofFunc, typeofVar :: Ident -> TypeM Type
typeofVar id = typeof (VIdent $ toStr id)
typeofFunc id = typeof (FIdent $ toStr id)
typeofMVar, typeofMFunc :: Type -> Ident -> TypeM Type
typeofMVar typ id = typeof' typ (VIdent $ toStr id)
typeofMFunc typ id = typeof' typ (FIdent $ toStr id)

checkTypes :: Stmt -> Either String (Type, Set.Set Type)
checkTypes = runTypeM typeenv0 . funS
  where
    funS :: Stmt -> TypeM Type
    funS stmt = case stmt of
      Global stmts -> undefined
      Local decls stmts -> undefined
      SDefFunc typ id args excepts stmt -> undefined
      SDeclVar typ id -> undefined
      SAssign id expr -> typeofVar id -||- funE expr
      SAssignOp id APlus expr ->
        (TInt -| typeofVar id -||- funE expr)
        `mplus`
        (TString -| typeofVar id -||- funE expr)
        `rethrow` Err.badArithType
      SAssignOp id opassign expr -> TInt -| typeofVar id -||- funE expr
      SAssignArr id expr1 expr2 -> funE (EAccessArr (EVar id) expr1) -||- funE expr2
      SPostInc id -> TString -| typeofVar id
      SPostDec id -> TString -| typeofVar id
      SReturn expr -> funE expr
      SIf expr stmt -> (TBool =| funE expr) >> funS stmt
      SIfElse expr stmt1 stmt2 -> (TBool =| funE expr) >> funS stmt1 >> funS stmt2
      SWhile expr stmt -> (TBool =| funE expr) >> funS stmt
      SForeach typ id expr stmt -> undefined
      SExpr expr -> funE expr >> return TVoid
      SThrow expr -> TString -| funE expr
      STryCatch stmt1 typ2 id3 stmt4 -> undefined
      SReturnV -> return TVoid
      SEmpty -> return TVoid
    funE :: Expr -> TypeM Type
    funE expr = case expr of
      EVar id -> typeofVar id
      ELitInt n -> return TInt
      ELitTrue -> return TBool
      ELitFalse -> return TBool
      ELitString str -> return TString
      ELitChar c -> return TChar
      ENull -> return TObject
      EAccessArr expr1 expr2 -> do
        TInt =| funE expr2 `rethrow` Err.indexType
        TArray elt <- funE expr1 `rethrow` Err.subscriptNonArray
        return elt
      EAccessFn expr id exprs -> do
        exprt <- funE expr
        exprst <- mapM funE exprs
        TFunc ret args excepts <- typeofMFunc exprt id
        forM excepts throws
        (args == exprst) `instead` (throwError $ Err.argumentsNotMatch args exprst)
        return ret
      EAccessVar expr id -> do
        exprt <- funE expr
        vart <- typeofMVar exprt id
        return vart
      EApp id exprs -> do
        exprst <- mapM funE exprs
        TFunc ret args excepts <- typeofFunc id
        forM excepts throws
        (args == exprst) `instead` (throwError $ Err.argumentsNotMatch args exprst)
        return ret
      ENewArr typ expr -> do
        TInt =| funE expr
        -- TODO temporary limit
        (typ `elem` [TInt, TBool, TChar]) `instead` (throwError "non-primitive array")
        return typ
      ENeg expr -> TInt =| funE expr
      ENot expr -> TBool =| funE expr
      EMul expr1 opmul2 expr3 -> TInt =| funE expr1 =||= funE expr3
      EAdd expr1 Plus expr3 ->
        (TInt -| funE expr1 -||- funE expr3)
        `mplus`
        (TString -| funE expr1 -||- funE expr3)
        `rethrow` Err.badArithType
      EAdd expr1 opadd2 expr3 -> TInt -| funE expr1 -||- funE expr3
      ERel expr1 oprel2 expr3 -> do
        funE expr1 =||= funE expr3
        (oprel2 `elem` [EQU, NEQ]) `instead` (TInt =| funE expr1)
        return TBool
      EAnd expr1 expr2 -> TBool =| funE expr1 =||= funE expr2
      EOr expr1 expr2 -> TBool =| funE expr1 =||= funE expr2


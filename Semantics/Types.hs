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
import Syntax.AbsJvmm (Arg(..), Type(..), Expr(..), Stmt(..), OpBin(..), OpUn(..))
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
  function :: Maybe Type,
  exceptions :: Set.Set Type,
  idents :: Types,
  -- TODO handle composite types
  types :: MemberTypes
}
typeenv0 = TypeEnv {
  function = Nothing,
  exceptions = Set.empty,
  idents = types0,
  types = membertypes0
}

-- Domain-specific dialect
type TypeM = ReaderT TypeEnv (ErrorT String Identity)
runTypeM :: TypeEnv -> TypeM a -> Either String a
runTypeM r m = runIdentity $ runErrorT $ runReaderT m r

lookupM :: (Ord a) => a -> Map.Map a b -> TypeM b
lookupM key map = lift $ case Map.lookup key map of
  Just val -> return val
  Nothing -> throwError noMsg

typeof :: UIdent -> TypeM Type
typeof uid = (asks idents >>= lookupM uid) `rethrow` Err.unknownSymbolType uid

typeof' :: Type -> UIdent -> TypeM Type
typeof' typ uid = (asks types >>= lookupM typ >>= lookupM uid) `rethrow` Err.unknownMemberType typ uid

throws :: Type -> TypeM ()
throws typ = do
  excepts <- asks exceptions
  unless (Set.member typ excepts) $ throwError $ Err.uncaughtException typ

catches :: Type -> TypeM a -> TypeM a
catches typ = local (\env -> env { exceptions = Set.insert typ (exceptions env) })

returns :: Type -> TypeM ()
returns typ = do
  Just (TFunc rett _ _) <- asks function `rethrow` Err.danglingReturn --FIXME this might not work
  typ =| return rett
  return ()

-- Declares symbol to be of a given type
declare :: UIdent -> Type -> TypeM a -> TypeM a
declare uid typ = local (\env -> env { idents = Map.insert uid typ (idents env) })

-- Note that this does not recurse into function body
declare' :: Stmt -> TypeM a -> TypeM a
declare' x = case x of
  SDefFunc typ id args excepts stmt -> declare (FIdent $ toStr id) (functype x) . sequential declare'' args
  SDeclVar typ id -> declare (VIdent $ toStr id) typ

declare'' :: Arg -> TypeM a -> TypeM a
declare'' (Arg typ id) = declare' (SDeclVar typ id)

-- Note that this looks up members but does not recurse in methods body
define' :: Stmt -> TypeM a -> TypeM a
define' x = Prelude.id --FIXME

sequential :: (b -> TypeM a -> TypeM a) -> [b] -> TypeM a -> TypeM a
sequential fun stmts = foldl (\acc stmt -> acc . (fun stmt)) Prelude.id stmts

call :: Type -> TypeM a -> TypeM a
call typ@(TFunc ret args excepts) = local (\env -> env { function = Just $ typ })

call' :: Stmt -> TypeM a -> TypeM a
call' = call . functype

functype :: Stmt -> Type
functype (SDefFunc typ _ args excepts _) = TFunc typ (map (\(Arg typ _) -> typ) args) excepts

-- Type arithmetic
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

-- TODO remove these when ready
typeofFunc, typeofVar :: Ident -> TypeM Type
typeofVar id = typeof (VIdent $ toStr id)
typeofFunc id = typeof (FIdent $ toStr id)
typeofMVar, typeofMFunc :: Type -> Ident -> TypeM Type
typeofMVar typ id = typeof' typ (VIdent $ toStr id)
typeofMFunc typ id = typeof' typ (FIdent $ toStr id)
-- TODO

checkTypes :: Stmt -> Either String Stmt
checkTypes = runTypeM typeenv0 . funS
  where
    funS :: Stmt -> TypeM Stmt
    funS x = case x of
      Global stmts -> sequential declare' stmts $ do
        stmts' <- mapM funS stmts
        return $ Global stmts'
      Local decls stmts -> sequential declare' decls $ do
        stmts' <- mapM funS stmts
        return $ Local decls stmts'
      SDefFunc typ id args excepts stmt -> do
        stmt' <- declare' x (call' x (funS stmt))
        return $ SDefFunc typ id args excepts stmt'
      SDeclVar typ id -> return x
      SAssign id expr -> do
        let expr' = expr --FIXME
        typeofVar id -||- funE expr
        return $ SAssign id expr'
      SAssignArr id expr1 expr2 -> do
        let expr1' = expr1 --FIXME
        let expr2' = expr2 --FIXME
        funE (EAccessArr (EVar id) expr1) -||- funE expr2
        return $ SAssignArr id expr1' expr2'
      SReturn expr -> do
        let expr' = expr --FIXME
        typ <- funE expr
        returns typ
        return $ SReturn expr'
      SIf expr stmt -> do
        let expr' = expr --FIXME
        TBool =| funE expr
        stmt' <- funS stmt
        return $ SIf expr' stmt'
      SIfElse expr stmt1 stmt2 -> do
        let expr' = expr --FIXME
        TBool =| funE expr
        stmt1' <- funS stmt1
        stmt2' <- funS stmt2
        return $ SIfElse expr' stmt1' stmt2'
      SWhile expr stmt -> do
        let expr' = expr --FIXME
        TBool =| funE expr
        stmt' <- funS stmt
        return $ SWhile expr' stmt'
      SForeach typ id expr stmt -> do
        let expr' = expr --FIXME
        TArray typ -| funE expr
        stmt' <- declare (VIdent $ toStr id) typ $ funS stmt
        return $ SForeach typ id expr' stmt'
      SExpr expr -> do
        let expr' = expr --FIXME
        funE expr
        return $ SExpr expr'
      SThrow expr -> do
        let expr' = expr --FIXME
        TString -| funE expr
        return $ SThrow expr'
      STryCatch stmt1 typ id stmt2 -> do
        -- TODO temporary limitation
        (typ `elem` [TString]) `instead` (throwError "bad exception type")
        stmt2' <- declare (VIdent $ toStr id) typ (funS stmt2)
        catches typ $ do
          stmt1' <- funS stmt1
          return $ STryCatch stmt1 typ id stmt2
      SReturnV -> do
        returns TVoid
        return x
      SEmpty -> return x
    funE :: Expr -> TypeM Type
    funE x = case x of
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
        return $ TArray typ
      EUnaryT _ op expr -> case op of
        Not -> TBool =| funE expr
        Neg -> TInt =| funE expr
      EBinaryT _ opbin expr1 expr2 -> do
        let typ = funE expr1 =||= funE expr2
        case opbin of
          Plus -> (TInt =| typ) `mplus` (TString =| typ) `rethrow` Err.badArithType
          And -> TBool =| typ
          Or -> TBool =| typ
          -- For primitives we have natural ==, for others we compare 'adresses'
          EQU -> return TBool
          NEQ -> return TBool
          -- TInt-only operations: Times Div Mod Minus LTH LEQ GTH GEQ
          _ -> do
            TInt =| typ
            case opbin of
              LTH -> return TBool
              LEQ -> return TBool
              GTH -> return TBool
              GEQ -> return TBool
              _ -> return TInt


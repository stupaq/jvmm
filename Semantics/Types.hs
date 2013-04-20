module Semantics.Types (staticTypes) where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Set as Set
import qualified Data.Map as Map
import Syntax.AbsJvmm (Ident, Type(..), Expr(..), Stmt(..), OpBin(..), OpUn(..))
import Semantics.Commons
import Semantics.Trans (UIdent(..), toStr)
import qualified Semantics.Errors as Err
import Semantics.Errors (rethrow)
import qualified Semantics.Scope as Scope

-- BUILTINS --
--------------
builtinGlobal = Map.fromList $ map (\(name, typ) -> (FIdent $ Scope.tagSymbol' Scope.tag0 name, typ)) [
    ("printInt", TFunc TVoid [TInt] []),
    ("readInt", TFunc TInt [] []),
    ("printString", TFunc TVoid [] []),
    ("readString", TFunc TString [] []),
    ("error", TFunc TVoid [] [])]

-- Typing is fully static. We type each null as Object type (which is a
-- superclass of every non-primitive value).
-- Note that after scope resolution we can't throw undeclared errors (also for
-- user defined types)

-- TYPE REPRESENTATION --
-------------------------
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
  idents = builtinGlobal,
  types = membertypes0
}

functype :: Stmt -> Type
functype (SDefFunc typ _ args excepts _) = TFunc typ (map (\(SDeclVar typ _) -> typ) args) excepts

-- TYPE MONAD --
----------------
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
-- FIXME ugly hack for now
typeof' (TArray _) (VIdent "length$0") = return TInt
typeof' typ uid = (asks types >>= lookupM typ >>= lookupM uid) `rethrow` Err.unknownMemberType typ uid

throws :: Type -> TypeM ()
throws typ = do
  excepts <- asks exceptions
  unless (Set.member typ excepts) $ throwError $ Err.uncaughtException typ

catches :: Type -> TypeM a -> TypeM a
catches typ = local (\env -> env { exceptions = Set.insert typ (exceptions env) })

returns :: Type -> TypeM ()
returns typ = do
  ftyp <- asks function 
  case ftyp of
    Just (TFunc rett _ _) -> typ =| rett >> return ()
    Nothing -> throwError Err.danglingReturn

-- Declares symbol to be of a given type locally
declare :: UIdent -> Type -> TypeM a -> TypeM a
declare uid typ = local (\env -> env { idents = Map.insert uid typ (idents env) })

-- Note that this does not recurse into function body
declare' :: Stmt -> TypeM a -> TypeM a
declare' x = case x of
  SDefFunc typ id args excepts stmt -> declare (FIdent $ toStr id) (functype x) . applyAndCompose declare' args
  SDeclVar typ id -> declare (VIdent $ toStr id) typ

-- Note that this looks up members but does not recurse in methods body
define' :: Stmt -> TypeM a -> TypeM a
define' x = Prelude.id -- TODO syntax for mutually recursive classes

call :: Type -> TypeM a -> TypeM a
call typ@(TFunc ret args excepts) = local (\env -> env { function = Just $ typ })

call' :: Stmt -> TypeM a -> TypeM a
call' = call . functype

-- TYPE ARITHMETIC --
---------------------
-- We say that t <- t1 =||= t2 when t2 and t1 are subtypes of t, and no other
-- type t' such that t =| t' has this property
(=||=) :: Type -> Type -> TypeM Type
(=||=) typ1 typ2 = do
  let bad = throwError (Err.unexpectedType typ1 typ2)
  (typ1 =| typ2) `mplus` (typ1 |= typ2) `mplus`
    case (typ1, typ2) of
      -- TODO hierarchy
      _ -> bad

-- We say that t1 =| t2 when t2 is a subtype of t1 (t2 can be safely casted to t1)
(=|), (|=) :: Type -> Type -> TypeM Type
(=|) typ1 typ2 = do
  let bad = throwError (Err.unexpectedType typ1 typ2)
      ok = return ()
  case (typ1, typ2) of
    (TArray etyp1, TArray etyp2) -> etyp1 =| etyp2 >> return ()
    (TInt, TInt) -> ok
    (TString, TString) -> ok
    (TChar, TChar) -> ok
    (TBool, TBool) -> ok
    (TObject, TObject) -> ok
    (TObject, TArray _) -> ok
    (TObject, TUser _) -> ok
    -- TODO hierarchy
    (TUser id1, TUser id2) -> bad
    _ -> bad
  return typ1

(|=) = flip (=|)

-- TODO remove these when ready
typeofFunc, typeofVar :: Ident -> TypeM Type
typeofVar id = typeof (VIdent $ toStr id)
typeofFunc id = typeof (FIdent $ toStr id)
typeofMVar, typeofMFunc :: Type -> Ident -> TypeM Type
typeofMVar typ id = typeof' typ (VIdent $ toStr id)
typeofMFunc typ id = typeof' typ (FIdent $ toStr id)
-- TODO

-- MAIN --
----------
staticTypes :: Stmt -> Either String Stmt
staticTypes = runTypeM typeenv0 . funS
  where
    funS :: Stmt -> TypeM Stmt
    funS x = case x of
      Global stmts -> applyAndCompose declare' stmts $ do
        stmts' <- mapM funS stmts
        return $ Global stmts'
      Local decls stmts -> applyAndCompose declare' decls $ do
        stmts' <- mapM funS stmts
        return $ Local decls stmts'
      SDefFunc typ id args excepts stmt -> do
        stmt' <- declare' x $ call' x $ applyAndCompose catches excepts $ funS stmt
        return $ SDefFunc typ id args excepts stmt'
      SDeclVar typ id -> return x
      SAssign id expr -> do
        (expr', etyp) <- funE expr
        vtyp <- typeofVar id
        vtyp =| etyp
        return $ SAssign id expr'
      SAssignArr id expr1 expr2 -> do
        (expr1', etyp1) <- funE expr1
        (expr2', etyp2) <- funE expr2
        TInt =| etyp1 `rethrow` Err.indexType
        atyp <- typeofVar id
        atyp =| TArray etyp2
        return $ SAssignArr id expr1' expr2'
      SReturn expr -> do
        (expr', etyp) <- funE expr
        returns etyp
        return $ SReturn expr'
      SIf expr stmt -> do
        (expr', etyp) <- funE expr
        TBool =| etyp
        stmt' <- funS stmt
        return $ SIf expr' stmt'
      SIfElse expr stmt1 stmt2 -> do
        (expr', etyp) <- funE expr
        TBool =| etyp
        stmt1' <- funS stmt1
        stmt2' <- funS stmt2
        return $ SIfElse expr' stmt1' stmt2'
      SWhile expr stmt -> do
        (expr', etyp) <- funE expr
        TBool =| etyp
        stmt' <- funS stmt
        return $ SWhile expr' stmt'
      SForeach typ id expr stmt -> do
        (expr', etyp) <- funE expr
        TArray typ =| etyp
        stmt' <- declare (VIdent $ toStr id) typ $ funS stmt
        return $ SForeach typ id expr' stmt'
      SExpr expr -> do
        (expr', _) <- funE expr
        return $ SExpr expr'
      SThrow expr -> do
        (expr', etyp) <- funE expr
        -- Temporary limit
        TString =| etyp
        throws TString
        return $ SThrow expr'
      STryCatch stmt1 typ id stmt2 -> do
        -- TODO temporary limitation
        (typ `elem` [TString]) `instead` (throwError "bad exception type")
        stmt2' <- declare (VIdent $ toStr id) typ (funS stmt2)
        catches typ $ do
          stmt1' <- funS stmt1
          return $ STryCatch stmt1' typ id stmt2'
      SReturnV -> do
        returns TVoid
        return x
      SEmpty -> return x
    funE :: Expr -> TypeM (Expr, Type)
    funE x = case x of
      EVar id -> do
        typ <- typeofVar id
        return (x, typ)
      ELitInt n -> return (x, TInt)
      ELitTrue -> return (x, TBool)
      ELitFalse -> return (x, TBool)
      ELitString str -> return (x, TString)
      ELitChar c -> return (x, TChar)
      ENull -> return (x, TObject)
      EAccessArr expr1 expr2 -> do
        (expr1', etyp1) <- funE expr1
        (expr2', etyp2) <- funE expr2
        TInt =| etyp2 `rethrow` Err.indexType
        case etyp1 of
          TArray typ -> return (EAccessArr expr1' expr2', typ)
          _ -> throwError Err.subscriptNonArray
      EAccessFn expr id exprs -> do
        (expr', etyp) <- funE expr
        (exprs', etypes) <- mapAndUnzipM funE exprs
        TFunc ret args excepts <- typeofMFunc etyp id
        forM excepts throws
        zipWithM_ (=|) args etypes `rethrow` Err.argumentsNotMatch args etypes
        return (EAccessFn expr' id exprs', ret)
      EAccessVar expr id -> do
        (expr', etyp) <- funE expr
        typ <- typeofMVar etyp id
        return (EAccessVar expr' id, typ)
      EApp id exprs -> do
        (exprs', etypes) <- mapAndUnzipM funE exprs
        TFunc ret args excepts <- typeofFunc id
        forM excepts throws
        zipWithM_ (=|) args etypes `rethrow` Err.argumentsNotMatch args etypes
        return (EApp id exprs', ret)
      ENewArr typ expr -> do
        -- TODO temporary limitation
        (typ `elem` [TInt, TBool, TChar]) `instead` (throwError "non-primitive array")
        (expr', etyp) <- funE expr
        TInt =| etyp
        return (ENewArr typ expr', TArray typ)
      EUnaryT _ op expr -> do
        (expr', etyp) <- funE expr
        case op of
          Not -> TBool =| etyp
          Neg -> TInt =| etyp
        return (EUnaryT etyp op expr', etyp)
      EBinaryT _ opbin expr1 expr2 -> do
        (expr1', etyp1) <- funE expr1
        (expr2', etyp2) <- funE expr2
        typ <- etyp1 =||= etyp2
        rett <- case opbin of
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
        return (EBinaryT rett opbin expr1' expr2', rett)


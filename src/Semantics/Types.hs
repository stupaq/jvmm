module Semantics.Types (staticTypes) where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Semantics.Commons
import Semantics.Errors (rethrow)
import qualified Semantics.Errors as Err
import Semantics.APTree
import qualified Semantics.Scope as Scope

-- BUILTINS --
--------------
entrypoint = (Scope.tagGlobal $ FIdent "main", TFunc TInt [] [])

builtinGlobal = Map.fromList $ map (\(name, typ) -> (Scope.tagGlobal $ FIdent name, typ)) [
    ("printInt", TFunc TVoid [TInt] []),
    ("readInt", TFunc TInt [] []),
    ("printString", TFunc TVoid [TString] []),
    ("readString", TFunc TString [] []),
    ("error", TFunc TVoid [] [])]

builtinMember typ uid = case (typ, uid) of
  (TArray _, VIdent "length$0") -> TInt
  (TString, VIdent "length$0") -> TInt
  (TString, FIdent "charAt$0") -> TFunc TChar [TInt] []
  _ -> TUnknown

builtinTypeNames = ["int", "char", "boolean", "String", "string"]

primitiveTypes = [TVoid, TInt, TChar, TBool]

-- Typing is fully static. Object type is a superclass of every non-primitive.
-- Note that after scope resolution we can't throw undeclared errors (also for
-- user defined types)

-- TYPE REPRESENTATION --
-------------------------
type Types = Map.Map UIdent Type
types0 = Map.empty

type MemberTypes = Map.Map Type Types
membertypes0 = Map.empty

data TypeEnv = TypeEnv {
  -- Type of a function currently executed
  function :: Maybe Type,
  -- Set of exceptions that are caught when throw in current context
  exceptions :: Set.Set Type,
  -- Types of identifiers
  idents :: Types,
  -- Definitions of types
  types :: MemberTypes
}
typeenv0 = TypeEnv {
  function = Nothing,
  exceptions = Set.empty,
  idents = builtinGlobal,
  types = membertypes0
}

setFunction x env = env { function = x }
mapExceptions fun env = env { exceptions = fun (exceptions env) }

typeof'' :: Stmt -> Type
typeof'' x = case x of
  SDefFunc typ _ args excepts _ -> TFunc typ (map (\(SDeclVar typ _) -> typ) args) excepts
  _ -> error $ Err.unusedBranch x

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
typeof' typ uid = case builtinMember typ uid of
  TUnknown -> (asks types >>= lookupM typ >>= lookupM uid) `rethrow` Err.unknownMemberType typ uid
  typ -> return typ

throws :: Type -> TypeM ()
throws typ = do
  excepts <- asks exceptions
  unless (Set.member typ excepts) $ throwError $ Err.uncaughtException typ

catches :: Type -> TypeM a -> TypeM a
catches typ = local (mapExceptions $ Set.insert typ)

returns :: Type -> TypeM ()
returns typ = do
  ftyp <- asks function 
  case ftyp of
    Just (TFunc rett _ _) -> rett =| typ >> return ()
    Nothing -> throwError Err.danglingReturn
    _ -> error $ Err.unusedBranch ftyp

decIdent :: UIdent -> Type -> TypeM a -> TypeM a
decIdent uid typ = local (\env -> env { idents = Map.insert uid typ (idents env) })

decType :: Type -> Types -> TypeM a -> TypeM a
decType typ tenv = local (\env -> env { types = Map.insert typ tenv (types env) })

-- Declares symbol to be of a given type or defines type
declare :: Stmt -> TypeM a -> TypeM a
declare x m = case x of
  SDefFunc typ id@(FIdent _) args excepts stmt ->
    decIdent id (typeof'' x) m
  SDeclVar typ id@(VIdent _) -> do
    when (typ == TVoid) $ throwError Err.voidVarDecl
    decIdent id typ m
  SDefClass id@(TIdent sid) super (SGlobal stmts) -> do
    when (sid `elem` builtinTypeNames) $ throwError (Err.redeclaredType id)
    -- We want to obtain all identifiers in class together with their types
    cls <- local (const typeenv0) . applyAndCompose declare stmts $ do
      env <- asks idents
      return env
    decType (TUser id) cls m
  _ -> error $ Err.unusedBranch x

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
      ok = return typ1
  case (typ1, typ2) of
    (TFunc _ argt1 _, TFunc _ argt2 _) -> do
      unless (length argt1 == length argt2) $ throwError noMsg
      zipWithM_ (=|) argt1 argt2
      ok
    (TInt, TInt) -> ok
    (TChar, TChar) -> ok
    (TBool, TBool) -> ok
    (TVoid, TVoid) -> ok
    (TNull, TNull) -> ok
    (TArray _, TNull) -> ok
    (TArray etyp1, TArray etyp2)
    -- This is an improvement when compared with Java
      | etyp1 == etyp2 -> ok
      | otherwise -> bad
    (TString, TNull) -> ok
    (TString, TString) -> ok
    (TObject, TNull) -> ok
    (TObject, TString) -> ok
    (TObject, TObject) -> ok
    (TObject, TArray _) -> ok
    (TObject, TUser _) -> ok
    (TUser _, TNull) -> ok
    -- TODO hierarchy
    (TUser id1, TUser id2)
      | id1 == id2 -> ok
      | otherwise -> bad
    _ -> bad

(|=) = flip (=|)

-- MAIN --
----------
staticTypes :: Stmt -> Either String Stmt
staticTypes = runTypeM typeenv0 . funS

funS :: Stmt -> TypeM Stmt
funS x = case x of
  SGlobal stmts -> applyAndCompose declare stmts $ do
    stmts' <- mapM funS stmts
    return $ SGlobal stmts'
  SLocal decls stmts -> applyAndCompose declare decls $ do
    stmts' <- mapM funS stmts
    return $ SLocal decls stmts'
  -- This is already delared when we see it in funS
  SDefFunc typ id args excepts stmt -> do
    let ftyp@(TFunc _ _ _) = typeof'' x
    when (id == fst entrypoint) $
      (snd entrypoint =| ftyp >> return ()) `rethrow` Err.incompatibleMain
    declare x . applyAndCompose declare args . applyAndCompose catches excepts $
      local (setFunction (Just ftyp)) $ do
        stmt' <- funS stmt
        return $ SDefFunc typ id args excepts stmt'
  -- This is already delared when we see it in funS
  SDeclVar typ id -> return x
  -- This is already delared when we see it in funS
  SDefClass id super (SGlobal stmts) -> do
    declare x $ do
      stmts' <- mapM funS stmts
      return $ SDefClass id super (SGlobal stmts')
  SDefClass _ _ _ -> error $ Err.unusedBranch x
  SAssign id expr -> do
    (expr', etyp) <- funE expr
    vtyp <- typeof id
    vtyp =| etyp
    return $ SAssign id expr'
  SAssignArr id expr1 expr2 -> do
    (expr1', etyp1) <- funE expr1
    (expr2', etyp2) <- funE expr2
    TInt =| etyp1 `rethrow` Err.indexType
    atyp <- typeof id
    atyp =| TArray etyp2
    return $ SAssignArr id expr1' expr2'
  SAssignFld ido idf expr -> do
    (expr', etyp) <- funE expr
    otyp <- typeof ido
    ftyp <- typeof' otyp idf
    ftyp =| etyp
    return $ SAssignFld ido idf expr'
  SReturn expr -> do
    (expr', etyp) <- funE expr
    when (etyp == TVoid) $ throwError Err.voidNotIgnored
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
  SExpr expr -> do
    (expr', _) <- funE expr
    return $ SExpr expr'
  SThrow expr -> do
    (expr', etyp) <- funE expr
    when (etyp `elem` primitiveTypes) $ throwError (Err.referencedPrimitive etyp)
    throws etyp
    return $ SThrow expr'
  STryCatch stmt1 typ id stmt2 -> do
    when (typ `elem` primitiveTypes) $ throwError (Err.referencedPrimitive typ)
    stmt2' <- decIdent id typ (funS stmt2)
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
    typ <- typeof id
    return (x, typ)
  ELitInt n -> return (x, TInt)
  ELitTrue -> return (x, TBool)
  ELitFalse -> return (x, TBool)
  ELitString str -> return (x, TString)
  ELitChar c -> return (x, TChar)
  ENull -> return (x, TNull)
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
    ftyp1@(TFunc ret args excepts) <- typeof' etyp id
    forM excepts throws
    (ftyp1 =| TFunc ret etypes []) `rethrow` Err.argumentsNotMatch args etypes
    return (EAccessFn expr' id exprs', ret)
  EAccessVar expr id -> do
    (expr', etyp) <- funE expr
    typ <- typeof' etyp id
    return (EAccessVar expr' id, typ)
  EApp id exprs -> do
    (exprs', etypes) <- mapAndUnzipM funE exprs
    ftyp1@(TFunc ret args excepts) <- typeof id
    forM excepts throws
    (ftyp1 =| TFunc ret etypes []) `rethrow` Err.argumentsNotMatch args etypes
    return (EApp id exprs', ret)
  ENewArr typ expr -> do
    when (typ == TVoid) $ throwError Err.voidNotIgnored
    (expr', etyp) <- funE expr
    TInt =| etyp
    return (ENewArr typ expr', TArray typ)
  ENewObj typ -> do
    -- Referenced type cannot be primitive
    when (typ `elem` primitiveTypes) $ throwError (Err.referencedPrimitive typ)
    return (ENewObj typ, typ)
  EUnary _ op expr -> do
    (expr', etyp) <- funE expr
    case op of
      OuNot -> TBool =| etyp
      OuNeg -> TInt =| etyp
    return (EUnary etyp op expr', etyp)
  EBinary _ opbin expr1 expr2 -> do
    (expr1', etyp1) <- funE expr1
    (expr2', etyp2) <- funE expr2
    typ <- etyp1 =||= etyp2
    rett <- case opbin of
      ObPlus -> (TInt =| typ) `mplus` (TString =| typ) `rethrow` Err.badArithType
      ObAnd -> TBool =| typ
      ObOr -> TBool =| typ
      -- For primitives we have natural ==, for others we compare 'adresses'
      ObEQU -> return TBool
      ObNEQ -> return TBool
      -- TInt-only operations: Times Div Mod Minus LTH LEQ GTH GEQ
      _ -> do
        TInt =| typ
        case opbin of
          ObLTH -> return TBool
          ObLEQ -> return TBool
          ObGTH -> return TBool
          ObGEQ -> return TBool
          _ -> return TInt
    return (EBinary rett opbin expr1' expr2', rett)


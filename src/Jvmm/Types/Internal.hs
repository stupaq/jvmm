module Jvmm.Types.Internal where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Traversable as Traversable

import qualified Jvmm.Errors as Err
import Jvmm.Errors (rethrow, ErrorInfoT)
import Jvmm.Builtins
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- DEBUG/
import Text.Show.Pretty (ppShow)
debug :: TypeM a
debug = do
  env <- ask
  error $ "\n" ++ ppShow env
-- /DEBUG

-- WEIRD STUFF --
-----------------
-- For MonadReader this can be easily used to collect and compose environments (e.g. when we collect
-- arguments definitions to create environment for function body).
applyAndCompose :: (b -> a -> a) -> [b] -> a -> a
applyAndCompose f = Prelude.foldl (flip (.)) Prelude.id . map f

-- TYPE REPRESENTATION --
-------------------------
type Types = Map.Map UIdent Type
types0 = Map.empty

type MemberTypes = Map.Map Type Types
membertypes0 = Map.empty

data TypeEnv = TypeEnv {
  -- Type of a function currently executed
  typeenvFunction :: Maybe Type,
  -- Set of exceptions that are caught when throw in current context
  typeenvExceptions :: Set.Set Type,
  -- Types of identifiers
  typeenvIdents :: Types,
  -- Definitions of types
  typeenvTypes :: MemberTypes,
  -- This type
  typeenvThis :: Maybe Type,
  -- Mapping from type to super
  typeenvSuper :: Map.Map Type Type
} deriving (Show)

typeenv0 = TypeEnv {
  typeenvFunction = Nothing,
  typeenvExceptions = Set.empty,
  typeenvIdents = types0,
  typeenvTypes = membertypes0,
  typeenvThis = Nothing,
  typeenvSuper = Map.empty
}

-- BUILDING TYPE ENVIRONMENT --
-------------------------------
-- Returns TypeEnv filled with type information about all classes in the hierarchy.
collectTypes :: ClassHierarchy -> ErrorInfoT Identity TypeEnv
collectTypes classes = fmap snd $ runStateT (Traversable.mapM decClass classes) typeenv0
  where
    decClass :: Class -> StateT TypeEnv (ErrorInfoT Identity) ()
    decClass clazz@Class { classType = typ, classSuper = super } = do
      when (isBuiltinType typ) $ throwError (Err.redeclaredType typ)
      modify $ \env -> env {
        typeenvTypes = Map.insert typ types (typeenvTypes env),
        typeenvSuper = Map.insert typ super (typeenvSuper env)
      }
      where
        fields = List.map (\x -> (fieldIdent x, fieldType x)) $ classFields clazz
        methods = List.map (\x -> (methodIdent x, methodType x)) $ classMethods clazz
        staticMethods = List.map (\x -> (methodIdent x, methodType x)) $ classStaticMethods clazz
        types = Map.fromList $ fields ++ methods ++ staticMethods

-- Typing is fully static. TObject type is a superclass of every non-primitive.
-- Note that after scope resolution we can't throw undeclared errors (also for
-- user defined types)

-- TYPE MONAD --
----------------
type TypeM = ReaderT TypeEnv (ErrorInfoT Identity)
runTypeM :: TypeEnv -> TypeM a -> ErrorInfoT Identity a
runTypeM env m = runReaderT m env

lookupM :: (Ord a) => a -> Map.Map a b -> TypeM b
lookupM key map = lift $ case Map.lookup key map of
  Just val -> return val
  Nothing -> throwError noMsg

typeof :: UIdent -> TypeM Type
typeof IThis = this
typeof uid = (asks typeenvIdents >>= lookupM uid) `rethrow` Err.unknownSymbolType uid

typeof' :: Type -> UIdent -> TypeM Type
typeof' typ uid = case builtinMemberType typ uid of
  TUnknown -> (asks typeenvTypes >>= lookupM typ >>= lookupM uid) `rethrow` Err.unknownMemberType typ uid
  typ -> return typ

typeof'' :: UIdent -> TypeM Type
typeof'' uid = this >>= flip typeof' uid

throws :: Type -> TypeM ()
throws typ = do
  excepts <- asks typeenvExceptions
  unless (Set.member typ excepts) $ throwError $ Err.uncaughtException typ

called :: Type -> [UIdent] -> TypeM a -> TypeM a
called typ@(TFunc returnType argumentTypes exceptions) argumentIdents action = do
  forM_ argumentTypes $ \argt -> notAVoid argt `rethrow` Err.voidArg
  catches exceptions . argumentsEnv . enterFunction typ $ action
  where
    argumentsEnv :: TypeM a -> TypeM a
    argumentsEnv = applyAndCompose (uncurry $ flip declare) $ List.zip argumentTypes argumentIdents
called x _ _ = throwError $ Err.unusedBranch x

catches :: [Type] -> TypeM a -> TypeM a
catches types = local (\env -> env { typeenvExceptions = List.foldl (flip Set.insert) (typeenvExceptions env) types })

returns :: Type -> TypeM ()
returns typ = do
  ftyp <- asks typeenvFunction 
  case ftyp of
    Just (TFunc rett _ _) -> rett =| typ >> return ()
    Nothing -> throwError Err.danglingReturn
    _ -> error $ Err.unusedBranch ftyp

declare :: UIdent -> Type -> TypeM a -> TypeM a
declare uid typ action = do
  notAVoid typ `rethrow` Err.voidVar uid
  local (\env -> env { typeenvIdents = Map.insert uid typ (typeenvIdents env) }) action

this :: TypeM Type
this = asks typeenvThis >>= \x -> case x of
  Just typ -> return typ
  Nothing -> throwError Err.danglingThis

super :: Type -> TypeM Type
super typ = (asks typeenvSuper >>= lookupM typ) `rethrow` Err.noSuperType typ

enterFunction, enterClass :: Type -> TypeM a -> TypeM a
enterFunction x = local $ \env -> env { typeenvFunction = Just x }
enterClass x = local $ \env -> env { typeenvThis = Just x }

invoke :: Type -> [Type] -> TypeM Type
invoke ftyp@(TFunc ret args excepts) etypes = do
    forM excepts throws
    (ftyp =| TFunc ret etypes []) `rethrow` Err.argumentsNotMatch args etypes
    return ret

-- COMMON ASSERTIONS --
-----------------------
notAVoid :: Type -> TypeM ()
notAVoid typ = when (typ == TVoid) $ throwError noMsg

intWithinBounds :: Integer -> TypeM ()
intWithinBounds n =
  when (n /= (fromIntegral (fromIntegral n :: Int) :: Integer)) $ throwError noMsg

-- TYPE ARITHMETIC --
---------------------
-- We say that t <- t1 =||= t2 when t2 and t1 are subtypes of t, and no other
-- type t' such that t =| t' has this property
(=||=) :: Type -> Type -> TypeM Type
(=||=) typ1 typ2 = 
  (typ1 =| typ2) `mplus`
  (typ1 |= typ2) `mplus`
  (super typ1 >>= (=||= typ2)) `mplus`
  (super typ2 >>= (typ1 =||=)) `rethrow` Err.unexpectedType typ1 typ2

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
    (TUser _, TUser _)
      | typ1 == typ2 -> ok
      | otherwise -> do
        typ2' <- super typ2
        typ1 =| typ2'
    _ -> bad

(|=) = flip (=|)

-- TRAVERSING TREE --
---------------------
funH :: ClassHierarchy -> TypeM ClassHierarchy
funH = Traversable.mapM $ \clazz@Class { classType = typ } ->
  enterClass typ $ do
    fields' <- mapM funF $ classFields clazz
    methods' <- mapM funM $ classMethods clazz
    staticMethods' <- mapM funMS $ classStaticMethods clazz
    return $ clazz {
          classMethods = methods'
        , classFields = fields'
        , classStaticMethods = staticMethods'
      }

funF :: Field -> TypeM Field
funF field@Field { fieldType = typ, fieldIdent = id } = do
  notAVoid typ `rethrow` Err.voidField id
  return field

funM :: Method -> TypeM Method
funM method@Method { methodType = typ, methodBody = stmt, methodArgs = args } = do
  called typ args $ do
    stmt' <- funS stmt
    return $ method { methodBody = stmt' }

funMS :: Method -> TypeM Method
funMS method = do
  checkEntrypoint method
  funM method
  where
    checkEntrypoint :: Method -> TypeM ()
    checkEntrypoint method =
      when (entrypointIdent == methodIdent method) $
        (entrypointType =| methodType method >> return ()) `rethrow` Err.incompatibleMain

funS :: Stmt -> TypeM Stmt
funS x = case x of
  SLocal vars stmts -> applyAndCompose (\(Variable typ id) -> declare id typ) vars $ do
    stmts' <- mapM funS stmts
    return $ SLocal vars stmts'
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
    notAVoid etyp `rethrow` Err.voidNotIgnored
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
    stmt2' <- declare id typ $ funS stmt2
    catches [typ] $ do
      stmt1' <- funS stmt1
      return $ STryCatch stmt1' typ id stmt2'
  SReturnV -> do
    returns TVoid
    return x
  SEmpty -> return x
  SBuiltin -> return x
  SInherited -> return x
  SDeclVar _ _ -> throwError $ Err.unusedBranch x

funE :: Expr -> TypeM (Expr, Type)
funE x = case x of
  EVar id -> do
    typ <- typeof id
    return (x, typ)
  ELitInt n -> do
    intWithinBounds n `rethrow` Err.intValueOutOfBounds n
    return (x, TInt)
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
  ECall id exprs -> do
    (exprs', etypes) <- mapAndUnzipM funE exprs
    ftyp <- typeof'' id
    rett <- invoke ftyp etypes
    return (ECall id exprs', rett)
  EAccessFn expr id exprs -> do
    (expr', etyp) <- funE expr
    (exprs', etypes) <- mapAndUnzipM funE exprs
    ftyp <- typeof' etyp id
    rett <- invoke ftyp etypes
    return (EAccessFn expr' id exprs', rett)
  EAccessVar expr id -> do
    (expr', etyp) <- funE expr
    typ <- typeof' etyp id
    return (EAccessVar expr' id, typ)
  ENewArr typ expr -> do
    notAVoid typ `rethrow` Err.voidNotIgnored
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
  EThis -> this >>= \typ -> return (EThis, typ)


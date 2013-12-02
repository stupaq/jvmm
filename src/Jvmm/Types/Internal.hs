{-# LANGUAGE TypeSynonymInstances #-}
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

-- WEIRD STUFF --
-----------------
-- For MonadReader this can be easily used to collect and compose environments (e.g. when we collect
-- arguments definitions to create environment for function body).
foldF :: (b -> a -> a) -> [b] -> a -> a
foldF f = Prelude.foldl (flip (.)) Prelude.id . map f

-- TYPE REPRESENTATION --
-------------------------
data Symbol =
    SFunction MethodName
  | SVariable VariableNum
  deriving (Show, Eq, Ord)

type Types = Map.Map Symbol Type
types0 = Map.empty

data Member =
    SMethod MethodName
  | SField FieldName
  deriving (Show, Eq, Ord)

type Members = Map.Map Member Type
members0 = Map.empty

type MemberTypes = Map.Map TypeComposed Members
membertypes0 = Map.empty

data TypeEnv = TypeEnv {
    -- Type of a function currently executed
    typeenvFunction :: Maybe TypeMethod
    -- This type
  , typeenvThis :: Maybe TypeComposed
    -- Set of exceptions that are caught when throw in current context
  , typeenvExceptions :: Set.Set TypeComposed
    -- Types of symbols
  , typeenvSymbols :: Types
    -- Definitions of types
  , typeenvTypes :: MemberTypes
    -- Mapping from type to super
  , typeenvSuper :: Map.Map TypeComposed TypeComposed
} deriving (Show)

typeenv0 = TypeEnv {
    typeenvFunction = Nothing
  , typeenvThis = Nothing
  , typeenvExceptions = Set.empty
  , typeenvSymbols = types0
  , typeenvTypes = membertypes0
  , typeenvSuper = Map.empty
}

typeenvNewSymbol :: Symbol -> Type -> TypeEnv -> TypeEnv
typeenvNewSymbol sym typ env = env { typeenvSymbols = Map.insert sym typ (typeenvSymbols env) }

typeenvNewType :: TypeComposed -> Members -> TypeEnv -> TypeEnv
typeenvNewType typ mem env = env { typeenvTypes = Map.insert typ mem (typeenvTypes env) }

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
          typeenvTypes = Map.insert typ typeDef (typeenvTypes env)
        , typeenvSuper = Map.insert typ super (typeenvSuper env)
        , typeenvSymbols = Map.fromList staticMethods
      }
      where
        fields = List.map (\x -> (SField $ fieldName x, toType $ fieldType x)) $ classFields clazz
        methods = List.map (\x -> (SMethod $ methodName x, toType $ methodType x)) $ classMethods clazz
        typeDef = Map.fromList $ fields ++ methods
        staticMethods = List.map (\x -> (SFunction $ methodName x, toType $ methodType x)) $ classStaticMethods clazz

-- Typing is fully static. TObject type is a superclass of every non-primitive.
-- Note that after scope resolution we can't throw undeclared errors (also for
-- user defined types)

-- TYPE MONAD --
----------------
type TypeM = ReaderT TypeEnv (ErrorInfoT Identity)
runTypeM :: TypeEnv -> TypeM a -> ErrorInfoT Identity a
runTypeM env m = runReaderT m env

lookupM :: (Ord a) => a -> Map.Map a b -> TypeM b
lookupM key map = case Map.lookup key map of
  Just val -> return val
  Nothing -> throwError noMsg

-- TYPE RESOLUTION PRIMITIVES --
--------------------------------
class Typeable a where
  typeof :: a -> TypeM Type

instance Typeable VariableNum where
  typeof num = (asks typeenvSymbols >>= lookupM (SVariable num)) `rethrow` Err.unknownSymbolType num

instance Typeable MethodName where
  typeof name = (asks typeenvSymbols >>= lookupM (SFunction name)) `rethrow` Err.unknownSymbolType name

class ComposedTypeable b where
  typeof' :: TypeComposed -> b -> TypeM Type

instance ComposedTypeable MethodName where
  typeof' typ name = builtinMethodType typ name
    `mplus` (asks typeenvTypes >>= lookupM typ >>= lookupM (SMethod name))
    `rethrow` Err.unknownMemberType typ name

instance ComposedTypeable FieldName where
  typeof' typ name = builtinFieldType typ name
    `mplus` (asks typeenvTypes >>= lookupM typ >>= lookupM (SField name))
    `rethrow` Err.unknownMemberType typ name

-- TYPE ALTERNATION PRIMITIVES --
---------------------------------
class Declarable a where
  declare :: a -> TypeM b -> TypeM b
  declareAll :: [a] -> TypeM b -> TypeM b
  declareAll = List.foldl (flip (.)) Prelude.id . List.map declare

instance Declarable Variable where
  declare (Variable typ num _) action = do
    notAVoid typ' `rethrow` Err.voidNotIgnored
    local (typeenvNewSymbol (SVariable num) typ') action
    where
      typ' = toType typ

-- TYPE ASSERTIONS --
---------------------
throws :: TypeComposed -> TypeM ()
throws typ = do
  excepts <- asks typeenvExceptions
  unless (Set.member typ excepts) $ throwError $ Err.uncaughtException typ

called :: TypeMethod -> [Variable] -> [Variable] -> TypeM a -> TypeM a
called typ@(TypeMethod returnType argumentTypes exceptions) arguments localVariables action = do
  forM_ argumentTypes $ \argt -> notAVoid (toType argt) `rethrow` Err.voidArg
  catches exceptions . declareAll arguments . declareAll localVariables . enterFunction typ $ action
called x _ _ _ = error $ Err.unusedBranch "attempt to call not a functional type"

catches :: [Type] -> TypeM a -> TypeM a
catches types = local (\env -> env {
      typeenvExceptions = List.foldl (flip Set.insert) (typeenvExceptions env) types
    })

returns :: Type -> TypeM ()
returns typ = do
  ftyp <- asks typeenvFunction
  case ftyp of
    Just (TMethod rett _ _) -> rett =| typ >> return ()
    Nothing -> throwError Err.danglingReturn
    _ -> error $ Err.unusedBranch "typeenvFunction was not of functional type"

this :: TypeM Type
this = asks typeenvThis >>= \x -> case x of
  Just typ -> return typ
  Nothing -> throwError Err.danglingThis

super :: Type -> TypeM Type
super typ = (asks typeenvSuper >>= lookupM typ) `rethrow` Err.noSuperType typ

enterFunction, enterInstance :: Type -> TypeM a -> TypeM a
enterFunction x = local $ \env -> env { typeenvFunction = Just x }
enterInstance x = local (\env -> env { typeenvThis = Just x }) . declare (Variable x variablenumThis variablename0)

invoke :: TypeMethod -> [Type] -> TypeM Type
invoke ftyp@(TMethod ret args excepts) etypes = do
    forM excepts throws
    (ftyp =| TypeMethod ret etypes []) `rethrow` Err.argumentsNotMatch args etypes
    return ret

notAVoid :: Type -> TypeM ()
notAVoid typ = when (typ == toType TVoid) $ throwError noMsg

notAPrimitive :: TypeBasic -> TypeM ()
notAPrimitive x = case x of
      TPrimitive _ -> throwError (Err.referencedPrimitive x)
      TComposed _ -> return ()

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
    (TypeMethod _ argt1 _, TypeMethod _ argt2 _) -> do
      unless (length argt1 == length argt2) $ throwError noMsg
      zipWithM_ (=|) argt1 argt2
      ok
    (TInt, TInt) -> ok
    (TChar, TChar) -> ok
    (TBool, TBool) -> ok
    (TVoid, TVoid) -> ok
    (TNull, TNull) -> ok
    (TArray _, TNull) -> ok
    -- Different people say different things about this
    (TArray etyp1, TArray etyp2) -> etyp1 =| etyp2
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
funH = Traversable.mapM $ \clazz@Class { classType = typ, classLocation = loc } ->
  Err.withLocation loc $ do
    staticMethods' <- mapM funMS $ classStaticMethods clazz
    enterInstance typ $ do
      fields' <- mapM funF $ classFields clazz
      methods' <- mapM funM $ classMethods clazz
      return $ clazz {
            classMethods = methods'
          , classFields = fields'
          , classStaticMethods = staticMethods'
        }

funF :: Field -> TypeM Field
funF field@Field { fieldType = typ, fieldName = id } = do
  notAVoid typ `rethrow` Err.voidField id
  return field

funM :: Method -> TypeM Method
funM method@Method { methodType = typ, methodBody = stmt, methodArgs = args, methodVariables = vars } = do
  Err.withLocation (methodLocation method) . called typ args vars $ do
    stmt' <- funS stmt
    return $ method { methodBody = stmt' }

funMS :: Method -> TypeM Method
funMS method = do
  checkEntrypoint method
  funM method
  where
    checkEntrypoint :: Method -> TypeM ()
    checkEntrypoint method =
      when (entrypointIdent == methodName method && methodOrigin method == TObject) $
        (entrypointType =| methodType method >> return ()) `rethrow` Err.incompatibleMain

funS :: Stmt -> TypeM Stmt
funS x = case x of
  SEmpty -> return x
  SBlock stmts -> do
    stmts' <- mapM funS stmts
    return $ SBlock stmts'
  SExpr expr -> do
    (expr', _) <- funE expr
    return $ SExpr expr'
  -- Memory access
  SStore num expr -> do
    (expr', etyp) <- funE expr
    vtyp <- typeof num
    vtyp =| etyp
    return $ SStore num expr'
  SStoreArray num expr1 expr2 -> do
    (expr1', etyp1) <- funE expr1
    (expr2', etyp2) <- funE expr2
    TInt =| etyp1 `rethrow` Err.indexType
    atyp <- typeof num
    atyp =| TArray etyp2
    return $ SStoreArray num expr1' expr2'
  SPutField num name expr -> do
    (expr', etyp) <- funE expr
    vtyp <- typeof num
    ftyp <- typeof' vtyp name
    ftyp =| etyp
    return $ SPutField num name expr'
  -- Control statements
  SReturn expr -> do
    (expr', etyp) <- funE expr
    notAVoid etyp `rethrow` Err.voidNotIgnored
    returns etyp
    return $ SReturn expr'
  SReturnV -> do
    returns TVoid
    return x
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
  SThrow expr -> do
    (expr', etyp) <- funE expr
    -- We cannot throw primitive type
    notAPrimitive etyp
    throws etyp
    return $ SThrow expr'
  STryCatch stmt1 typ num stmt2 -> do
    stmt2' <- funS stmt2
    catches [typ] $ do
      stmt1' <- funS stmt1
      return $ STryCatch stmt1' typ num stmt2'
  -- Special function bodies
  SBuiltin -> return x
  SInherited -> return x
  -- Metainformation carriers
  SMetaLocation loc stmts -> stmtMetaLocation loc $ mapM funS stmts
  -- These statements will be replaced with ones caring more context in subsequent phases
  T_SDeclVar _ _ -> error $ Err.unusedBranch x
  T_SAssign _ _ -> error $ Err.unusedBranch x
  T_SAssignArr _ _ _ -> error $ Err.unusedBranch x
  T_SAssignFld _ _ _ -> error $ Err.unusedBranch x
  T_STryCatch _ _ _ _ -> error $ Err.unusedBranch x

funE :: Expr -> TypeM (Expr, Type)
funE x = case x of
  -- Literals
  ENull -> return (x, TNull)
  ELitTrue -> return (x, TBool)
  ELitFalse -> return (x, TBool)
  ELitChar c -> return (x, TChar)
  ELitString str -> return (x, TString)
  ELitInt n -> do
    intWithinBounds n `rethrow` Err.intValueOutOfBounds n
    return (x, TInt)
  -- Memory access
  ELoad num -> fmap ((,) $ ELoad num) $ typeof num
  EArrayLoad expr1 expr2 -> do
    (expr1', etyp1) <- funE expr1
    (expr2', etyp2) <- funE expr2
    TInt =| etyp2 `rethrow` Err.indexType
    case etyp1 of
      TArray typ -> return (EArrayLoad expr1' expr2', typ)
      _ -> throwError Err.subscriptNonArray
  EGetField expr id -> do
    (expr', etyp) <- funE expr
    typ <- typeof' etyp id
    return (EGetField expr' id, typ)
  -- Method calls
  EInvokeStatic id exprs -> do
    (exprs', etypes) <- mapAndUnzipM funE exprs
    ftyp <- typeof id
    rett <- invoke ftyp etypes
    return (EInvokeStatic id exprs', rett)
  EInvokeVirtual expr id exprs -> do
    (expr', etyp) <- funE expr
    (exprs', etypes) <- mapAndUnzipM funE exprs
    ftyp <- typeof' etyp id
    rett <- invoke ftyp etypes
    return (EInvokeVirtual expr' id exprs', rett)
  -- Object creation
  ENewObj typ -> do
    -- We cannot allocate a primitive on the heap
    notAPrimitive typ
    return (ENewObj typ, typ)
  ENewArr typ expr -> do
    notAVoid typ `rethrow` Err.voidNotIgnored
    (expr', etyp) <- funE expr
    TInt =| etyp `rethrow` Err.indexType
    return (ENewArr typ expr', TArray typ)
  -- Operations
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
  -- These expressions will be replaced with ones caring more context in subsequent phases
  T_EVar _ -> error $ Err.unusedBranch x


{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Jvmm.Types.Internal where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Char as Char
import Data.Int (Int32)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable

import Jvmm.Builtins
import Jvmm.Errors (ErrorInfoT, orThrow, rethrow)
import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output

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

types0 :: Types
types0 = Map.empty

data Member =
    SMethod MethodName
  | SField FieldName
  deriving (Show, Eq, Ord)

type Members = Map.Map Member Type

members0 :: Members
members0 = Map.empty

type MemberTypes = Map.Map TypeComposed Members

membertypes0 :: MemberTypes
membertypes0 = Map.empty

data TypeEnv = TypeEnv {
    -- Type of a function currently executed
    typeenvFunction      :: Maybe TypeMethod
    -- This type
  , typeenvThis          :: Maybe TypeComposed
    -- Set of exceptions that are caught when throw in current context
  , typeenvExceptions    :: Set.Set TypeComposed
    -- Types of symbols
  , typeenvSymbols       :: Types
    -- Definitions of types
  , typeenvTypes         :: MemberTypes
    -- Mapping from type to super
  , typeenvSuper         :: Map.Map TypeComposed TypeComposed
    -- Origin of visible static methods
  , typeenvStaticOrigin  :: TypeComposed
    -- Expected type for limited type inference
  , typeenvExpectedTypes :: [TypeBasic]
} deriving (Show)

typeenv0 :: TypeEnv
typeenv0 = TypeEnv {
    typeenvFunction = Nothing
  , typeenvThis = Nothing
  , typeenvExceptions = Set.empty
  , typeenvSymbols = types0
  , typeenvTypes = membertypes0
  , typeenvSuper = Map.empty
  , typeenvStaticOrigin = undefined
  , typeenvExpectedTypes = []
}

typeenvNewSymbol :: Symbol -> Type -> TypeEnv -> TypeEnv
typeenvNewSymbol sym typ env = env { typeenvSymbols = Map.insert sym typ (typeenvSymbols env) }

typeenvNewType :: TypeComposed -> Members -> TypeEnv -> TypeEnv
typeenvNewType typ mem env = env { typeenvTypes = Map.insert typ mem (typeenvTypes env) }

-- BUILDING TYPE ENVIRONMENT --
-------------------------------
-- Returns TypeEnv filled with type information about all classes in the hierarchy.
collectTypes :: ClassHierarchy -> ErrorInfoT Identity TypeEnv
collectTypes classes = snd <$> runStateT (Traversable.mapM decClass classes) typeenv0
  where
    decClass :: Class -> StateT TypeEnv (ErrorInfoT Identity) ()
    decClass clazz@Class { classType = typ } = do
      when (isBuiltinType typ) $ throwError (Err.redeclaredType typ)
      modify $ \env -> env {
          typeenvTypes = Map.insert typ typeDef (typeenvTypes env)
        , typeenvSuper = Map.insert typ (classSuper clazz) (typeenvSuper env)
        , typeenvSymbols = Map.fromList staticMethods
      }
      where
        fields = List.map (\x -> (SField $ fieldName x, toType $ fieldType x)) $ classFields clazz
        methods = List.map (\x -> (SMethod $ methodName x, toType $ methodType x)) $ classInstanceMethods clazz
        typeDef = Map.fromList $ fields ++ methods
        staticMethods = List.map (\x -> (SFunction $ methodName x, toType $ methodType x)) $ classStaticMethods clazz

-- TYPE MONAD --
----------------
type TypeM = ReaderT TypeEnv (ErrorInfoT Identity)
runTypeM :: TypeEnv -> TypeM a -> ErrorInfoT Identity a
runTypeM env m = runReaderT m env

lookupM :: (Ord a) => a -> Map.Map a b -> TypeM b
lookupM key aMap = case Map.lookup key aMap of
  Just val -> return val
  Nothing -> throwError noMsg

lookupS :: Symbol -> TypeM Type
lookupS sym = asks typeenvSymbols >>= lookupM sym

lookupC :: TypeComposed -> Member -> TypeM Type
lookupC typ sym = asks typeenvTypes >>= lookupM typ >>= lookupM sym

-- TYPE RESOLUTION PRIMITIVES --
--------------------------------
class Typeable a b where
  typeof :: a -> TypeM b

instance Typeable VariableNum TypeBasic where
  typeof VariableThis = liftM TComposed this
  typeof num@(VariableNum _) = orThrow (Err.unknownSymbolType num) $ do
    TBasic typ <- lookupS (SVariable num)
    return typ

instance Typeable MethodName TypeMethod where
  typeof name = orThrow (Err.unknownSymbolType name) $ do
    TMethod typ <- lookupS (SFunction name)
    return typ

instance Typeable VariableNum TypeComposed where
  typeof VariableThis = this
  typeof num = do
    typ <- typeof num
    notAPrimitive typ `rethrow` Err.notAComposedType typ

class ComposedTypeable a b c where
  typeof' :: a -> b -> TypeM c

instance ComposedTypeable TypeComposed MethodName TypeMethod where
  typeof' typ name@(MethodName str) = orThrow (Err.unknownMemberType typ name) $ do
    TMethod mtyp <- builtinMethodType (typ, str)
        `mplus` lookupC typ (SMethod name)
    return mtyp

instance ComposedTypeable TypeComposed FieldName TypeBasic where
  typeof' typ name@(FieldName str) = orThrow (Err.unknownMemberType typ name) $ do
    TBasic ftyp <- builtinFieldType (typ, str)
        `mplus` lookupC typ (SField name)
    return ftyp

instance ComposedTypeable TypeBasic FieldName TypeBasic where
  typeof' (TComposed typ) name = typeof' typ name
  typeof' typ name = throwError $ Err.unknownMemberType typ name

instance ComposedTypeable TypeBasic MethodName TypeMethod where
  typeof' (TComposed typ) name = typeof' typ name
  typeof' typ name = throwError $ Err.unknownMemberType typ name

-- Completely wrong queries
instance ComposedTypeable TypeBasic MethodName TypeBasic where
  typeof' typ name = throwError $ Err.unknownMemberType typ name

instance ComposedTypeable TypeComposed MethodName TypeBasic where
  typeof' typ name = throwError $ Err.unknownMemberType typ name

-- TYPE ALTERNATION PRIMITIVES --
---------------------------------
class Declarable a where
  declare :: a -> TypeM b -> TypeM b
  declareAll :: [a] -> TypeM b -> TypeM b
  declareAll = List.foldl (flip (.)) Prelude.id . List.map declare

instance Declarable Variable where
  declare (Variable typ num _) action = do
    notAVoid typ'
    local (typeenvNewSymbol (SVariable num) typ') action
    where
      typ' = toType typ

-- TYPE CHECKING AND CHANGING HELPERS --
----------------------------------------
throws :: TypeComposed -> TypeM ()
throws typ = do
  excepts <- asks typeenvExceptions
  unless (Set.member typ excepts) $ throwError $ Err.uncaughtException typ

called :: TypeMethod -> [Variable] -> [Variable] -> TypeM a -> TypeM a
called typ@(TypeMethod _ argumentTypes exceptions) arguments localVariables action = do
  forM_ argumentTypes $ \argt -> notAVoid (toType argt) `rethrow` Err.voidArg
  catches exceptions . declareAll arguments . declareAll localVariables . enterFunction typ $ action

catches :: [TypeComposed] -> TypeM a -> TypeM a
catches types = local (\env -> env {
      typeenvExceptions = List.foldl (flip Set.insert) (typeenvExceptions env) types
    })

returns :: TypeBasic -> TypeM TypeBasic
returns typ = do
  ftyp <- asks typeenvFunction
  case ftyp of
    Just (TypeMethod rett _ _) -> rett =| typ >> return rett
    Nothing -> throwError Err.danglingReturn

this :: TypeM TypeComposed
this = asks typeenvThis >>= \x -> case x of
  Just typ -> return typ
  Nothing -> throwError Err.danglingThis

enterFunction :: TypeMethod -> TypeM a -> TypeM a
enterFunction x = local $ \env -> env { typeenvFunction = Just x }

enterClass :: TypeComposed -> TypeM a -> TypeM a
enterClass x = local (\env -> env { typeenvStaticOrigin = x })

enterInstance :: TypeComposed -> TypeM a -> TypeM a
enterInstance x =
  local (\env -> env { typeenvThis = Just x })

invoke :: TypeMethod -> [TypeBasic] -> TypeM TypeBasic
invoke ftyp@(TypeMethod ret args excepts) etypes = do
    forM_ excepts throws
    (ftyp =| TypeMethod ret etypes []) `rethrow` Err.argumentsNotMatch args etypes
    return ret

expectedType :: TypeBasic -> TypeM a -> TypeM a
expectedType typ = local $ \e -> e { typeenvExpectedTypes = typ : typeenvExpectedTypes e }

-- TYPE ASSERTIONS --
---------------------
class Assertable a where
  notAVoid :: a -> TypeM ()

instance (Show a, InheritsType a) => Assertable a where
  notAVoid typ = when (toType typ == toType TVoid) $ throwError Err.voidNotIgnored

notAPrimitive :: TypeBasic -> TypeM TypeComposed
notAPrimitive x = case x of
  TPrimitive _ -> throwError (Err.referencedPrimitive x)
  TComposed typ -> return typ

intWithinBounds :: Integer -> TypeM ()
intWithinBounds n =
  when (n /= (fromIntegral (fromIntegral n :: Int32) :: Integer)) $ throwError noMsg

-- TYPE ARITHMETIC --
---------------------
class (Show a) => Arithmetizable a where
  -- We say that t <- t1 =||= t2 when t2 and t1 are subtypes of t, and no other
  -- type t' such that t =| t' has this property
  (=||=) :: a -> a -> TypeM a
  (=||=) typ1 typ2 =
    (typ1 =| typ2) `mplus`
    (typ1 |= typ2) `mplus`
    (super typ1 >>= (=||= typ2)) `mplus`
    (super typ2 >>= (typ1 =||=)) `rethrow` Err.unexpectedType typ1 typ2
  -- We say that t1 =| t2 when t2 is a subtype of t1 (t2 can be safely casted to t1)
  (=|), (|=) :: a -> a -> TypeM a
  (|=) = flip (=|)
  super :: a -> TypeM a

instance Arithmetizable Type where
  (=|) typ1 typ2 = do
    let bad = throwError (Err.unexpectedType typ1 typ2)
    case (typ1, typ2) of
      (TMethod typ1', TMethod typ2') -> liftM TMethod $ typ1' =| typ2'
      (TBasic typ1', TBasic typ2') -> liftM TBasic $ typ1' =| typ2'
      _ -> bad
  super (TBasic typ) = liftM TBasic $ super typ
  super typ = throwError $ Err.notAComposedType typ

instance Arithmetizable TypeMethod where
  (=|) typ1 typ2 = do
    let (TypeMethod _ argt1 _, TypeMethod _ argt2 _) = (typ1, typ2)
    unless (length argt1 == length argt2) $ throwError noMsg
    zipWithM_ (=|) argt1 argt2
    return typ1
  super = Err.unreachable "method type is not hierarchical"

instance Arithmetizable TypeBasic where
  (=|) targ1 targ2 = do
    let bad = throwError (Err.unexpectedType targ1 targ2)
    case (targ1, targ2) of
      (TPrimitive typ1, TPrimitive typ2) -> liftM TPrimitive $ typ1 =| typ2
      (TComposed typ1, TComposed typ2) -> liftM TComposed $ typ1 =| typ2
      _ -> bad
  super (TComposed typ) = liftM TComposed $ super typ
  super typ = throwError $ Err.notAComposedType typ

instance Arithmetizable TypePrimitive where
  (=|) typ1 typ2 = do
    let bad = throwError (Err.unexpectedType typ1 typ2)
        ok = return typ1
    case (typ1, typ2) of
      (TInt, TInt) -> ok
      (TChar, TChar) -> ok
      (TBool, TBool) -> ok
      (TVoid, TVoid) -> ok
      _ -> bad
  super = Err.unreachable "primitive type is not hierarchical"

instance Arithmetizable TypeComposed where
  (=|) typ1 typ2 = do
    let bad = throwError (Err.unexpectedType typ1 typ2)
        ok = return typ1
    case (typ1, typ2) of
      -- Different people say different things about this
      (TArray etyp1, TArray etyp2) -> liftM TArray $ etyp1 =| etyp2
      (TString, TString) -> ok
      (TObject, TString) -> ok
      (TObject, TObject) -> ok
      (TObject, TArray _) -> ok
      (TObject, TUser _) -> ok
      (TUser _, TUser _)
        | typ1 == typ2 -> ok
        | otherwise -> do
          typ2' <- super typ2 `catchError` const bad
          typ1 =| typ2'
          ok
      _ -> bad
  super typ = do
    styp <- asks typeenvSuper >>= lookupM typ
    guard (styp /= typ)
    return styp

(=?) :: (InheritsType a, InheritsType b) => a -> b -> TypeM a
(=?) typ1 typ2 = toType typ1 =| toType typ2 >> return typ1
(?=) :: (InheritsType a, InheritsType b) => a -> b -> TypeM b
(?=) = flip (=?)

-- TRAVERSING TREE --
---------------------
class TypeCheckable a where
  tcheck :: a -> TypeM a

instance TypeCheckable ClassHierarchy where
  tcheck = Traversable.mapM $ \clazz@Class { classType = typ, classLocation = loc } ->
    Err.withLocation loc $ enterClass typ $ do
      staticMethods' <- mapM tcheck $ classStaticMethods clazz
      enterInstance typ $ do
        fields' <- mapM tcheck $ classFields clazz
        instanceMethods' <- mapM tcheck $ classInstanceMethods clazz
        return $ clazz {
              classAllMethods = instanceMethods' ++ staticMethods'
            , classFields = fields'
          }

instance TypeCheckable Field where
  tcheck field@Field { fieldType = typ, fieldName = name } = do
    notAVoid typ `rethrow` Err.voidField name
    return field

instance TypeCheckable Method where
  tcheck method@Method { methodType = typ, methodBody = stmt, methodArgs = args,
      methodVariables = vars } =
    Err.withLocation (methodLocation method) . called typ args vars $ do
      checkEntrypoint method
      stmt' <- tcheck stmt
      return $ method { methodBody = stmt' }
    where
      checkEntrypoint :: Method -> TypeM ()
      checkEntrypoint aMethod = when (isEntrypoint aMethod)
          (void $ entrypointType =| methodType aMethod) `rethrow` Err.incompatibleMain

instance TypeCheckable Stmt where
  tcheck x = case x of
    SEmpty -> return x
    SBlock stmts -> do
      stmts' <- mapM tcheck stmts
      return $ SBlock stmts'
    SExpr expr _ -> do
      (expr', typ') <- tcheck' expr
      return $ SExpr expr' typ'
    -- Memory access
    SAssign lval expr _ -> do
      (lval', ltyp) <- tcheck' lval
      expectedType ltyp $ do
        (expr', etyp) <- tcheck' expr
        typ <- ltyp =| etyp
        return $ SAssign lval' expr' typ
    -- Control statements
    SReturn expr _ -> do
      (expr', etyp) <- tcheck' expr
      notAVoid etyp `rethrow` Err.voidNotIgnored
      tret <- returns etyp
      return $ SReturn expr' tret
    SReturnV -> do
      returns $ TPrimitive TVoid
      return x
    SIf expr stmt -> do
      (expr', etyp) <- tcheck' expr
      TBool =? etyp
      stmt' <- tcheck stmt
      return $ SIf expr' stmt'
    SIfElse expr stmt1 stmt2 -> do
      (expr', etyp) <- tcheck' expr
      TBool =? etyp
      stmt1' <- tcheck stmt1
      stmt2' <- tcheck stmt2
      return $ SIfElse expr' stmt1' stmt2'
    SWhile expr stmt -> do
      (expr', etyp) <- tcheck' expr
      TBool =? etyp
      stmt' <- tcheck stmt
      return $ SWhile expr' stmt'
    SThrow expr -> do
      (expr', etyp) <- tcheck' expr
      -- We cannot throw primitive type
      ctyp <- notAPrimitive etyp
      throws ctyp
      return $ SThrow expr'
    STryCatch stmt1 typ num stmt2 -> do
      stmt2' <- tcheck stmt2
      catches [typ] $ do
        stmt1' <- tcheck stmt1
        return $ STryCatch stmt1' typ num stmt2'
    -- Special function bodies
    SBuiltin -> return x
    SInherited -> return x
    -- Metainformation carriers
    SMetaLocation loc stmts -> stmtMetaLocation loc $ mapM tcheck stmts
    -- These statements will be replaced with ones caring more context in subsequent phases
    PruneSDeclVar {} -> Err.unreachable x
    PruneSTryCatch {} -> Err.unreachable x

class TypeCheckable' a where
  tcheck' :: a -> TypeM (a, TypeBasic)

instance TypeCheckable' RValue where
  tcheck' x = case x of
    -- Literals
    ENull typ -> return (x, TComposed typ)
    ELitTrue -> return (x, TPrimitive TBool)
    ELitFalse -> return (x, TPrimitive TBool)
    ELitChar c -> do
      unless (Char.isAscii c) $ throwError (Err.nonAsciiCharacter c)
      return (x, TPrimitive TChar)
    ELitString str -> do
      unless (all Char.isAscii str) $ throwError (Err.nonAsciiCharacter ' ')
      return (x, TComposed TString)
    ELitInt n -> do
      intWithinBounds n `rethrow` Err.intValueOutOfBounds n
      return (x, TPrimitive TInt)
    -- Memory access
    ELoad num _ -> do
      typ <- typeof num
      return (ELoad num typ, typ)
    EArrayLoad expr1 expr2 _ -> do
      (expr1', etyp1) <- tcheck' expr1
      (expr2', etyp2) <- tcheck' expr2
      TInt =? etyp2 `rethrow` Err.indexType
      case etyp1 of
        TComposed (TArray typ) -> return (EArrayLoad expr1' expr2' typ, typ)
        _ -> throwError Err.subscriptNonArray
    EGetField expr _ name _ -> do
      (expr', etyp) <- tcheck' expr
      ctyp <- notAPrimitive etyp
      typ <- typeof' ctyp name
      return (EGetField expr' ctyp name typ, typ)
    -- Method calls
    EInvokeStatic _ name _ exprs -> do
      (exprs', etypes) <- mapAndUnzipM tcheck' exprs
      ftyp <- typeof name
      rtyp <- invoke ftyp etypes
      styp <- asks typeenvStaticOrigin
      return (EInvokeStatic styp name ftyp exprs', rtyp)
    EInvokeVirtual expr _ name _ exprs -> do
      (expr', etyp) <- tcheck' expr
      ctyp <- notAPrimitive etyp
      (exprs', etypes) <- mapAndUnzipM tcheck' exprs
      ftyp <- typeof' ctyp name
      rtyp <- invoke ftyp etypes
      return (EInvokeVirtual expr' ctyp name ftyp exprs', rtyp)
    -- Object creation
    ENewObj typ -> return (ENewObj typ, TComposed typ)
    ENewArr typ expr -> do
      notAVoid typ
      (expr', etyp) <- tcheck' expr
      TInt =? etyp `rethrow` Err.indexType
      return (ENewArr typ expr', TComposed (TArray typ))
    -- Operations
    EUnary op expr _ -> do
      (expr', etyp) <- tcheck' expr
      case op of
        OuNot -> TBool =? etyp
        OuNeg -> TInt =? etyp
      return (EUnary op expr' etyp, etyp)
    EBinary opbin expr1 expr2 _ _ _ -> do
      (expr1', etyp1) <- tcheck' expr1
      (expr2', etyp2) <- tcheck' expr2
      typ <- etyp1 =||= etyp2
      isString <- Err.succeeded (TString =? typ)
      case (isString, opbin) of
        (True, ObPlus) ->
          let rett = TComposed TString
          in return (EBinary opbin expr1' expr2' rett etyp1 etyp2, rett)
        _ -> do
          rett <- liftM TPrimitive $ case opbin of
            ObAnd -> TBool =? typ
            ObOr -> TBool =? typ
            -- For primitives we have natural ==, for others we compare 'adresses'
            ObEQU -> return TBool
            ObNEQ -> return TBool
            -- TInt-only operations: Times Div Mod Minus LTH LEQ GTH GEQ
            _ -> do
              TInt =? typ
              case opbin of
                ObLTH -> return TBool
                ObLEQ -> return TBool
                ObGTH -> return TBool
                ObGEQ -> return TBool
                _ -> return TInt
          return (EBinary opbin expr1' expr2' rett etyp1 etyp2, rett)
    -- These expressions will be replaced with ones caring more context in subsequent phases
    PruneEVar {} -> Err.unreachable x
    PruneENull -> do
      types <- asks typeenvExpectedTypes
      case types of
        [TComposed etyp] -> tcheck' $ ENull etyp
        _ -> tcheck' $ ENull TObject

instance TypeCheckable' LValue where
  tcheck' lval@(PruneLExpr _) = Err.unreachable lval
  tcheck' lval = do
    (expr', typ) <- tcheck' $ toRValue lval
    return (toLValue expr', typ)


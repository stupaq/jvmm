{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Jvmm.LlvmEmitter.Internal where
import Jvmm.LlvmEmitter.Layout
import Jvmm.LlvmEmitter.Output

import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Traversable
import Data.Word

import qualified Jvmm.Builtins as Builtins
import Jvmm.Errors (ErrorInfoT)
import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output

import LLVM.General.AST as Llvm hiding (Type)
import LLVM.General.AST.AddrSpace as Llvm.AddrSpace
import LLVM.General.AST.CallingConvention as Llvm.CallingConvention
import LLVM.General.AST.Constant as Llvm.Constant
import LLVM.General.AST.Global as Llvm.Global
import LLVM.General.AST.Instruction as Llvm.Instruction
import LLVM.General.AST.IntegerPredicate as Llvm.IntegerPrediacte
import LLVM.General.AST.Linkage as Llvm.Linkage
import LLVM.General.AST.Name as Llvm.Name
import LLVM.General.AST.Operand as Llvm.Operand
import LLVM.General.AST.RMWOperation as Llvm.RMWOperation
import LLVM.General.AST.Type as Llvm.Type hiding (Type)
import qualified LLVM.General.AST.Type as Llvm.Type (Type)

type LlvmType = Llvm.Type.Type

-- LLVM RUNTIME AND BUILTINS --
-------------------------------
callDefaults :: CallableOperand -> [Operand] -> Instruction
callDefaults fun args = Call {
      isTailCall = False
    , Llvm.Instruction.callingConvention = Llvm.CallingConvention.C
    , Llvm.Instruction.returnAttributes = []
    , function = fun
    , arguments = [(op, []) | op <- args]
    , Llvm.Instruction.functionAttributes = []
    , metadata = []
  }

static, dynamic :: Name -> CallableOperand
static = Right . ConstantOperand . GlobalReference
dynamic = Right . LocalReference

defAddrSpace :: AddrSpace
defAddrSpace = AddrSpace 0

boolType, intType, charType, voidPtrType, stringType :: LlvmType
boolType = IntegerType 1
intType = IntegerType 32
charType = IntegerType 8
voidPtrType = PointerType charType defAddrSpace
stringType = PointerType charType defAddrSpace

intValue :: Integer -> Constant
intValue int = Llvm.Constant.Int 32 int
charValue :: Char -> Constant
charValue int = Llvm.Constant.Int 8 $ toInteger $ Char.ord int

trueConst, falseConst :: Constant
trueConst = Llvm.Constant.Int 1 1
falseConst = Llvm.Constant.Int 1 0

alignRef, alignPrim, align0 :: Word32
alignRef = 0 -- 8
alignPrim = 0 -- 4
align0 = 0

composedName :: TypeComposed -> Name
composedName x = case x of
  TObject -> Name "class.Object"
  TUser (ClassName str) -> Name $ "class." ++ str
  _ -> Err.unreachable "type has no name"

(+/+) :: Name -> String -> Name
(+/+) (Name pref) suf = Name $ pref ++ "." ++ suf
(+/+) x@(UnName _) _ = x

objectProto :: TypeComposed -> Name
objectProto typ = composedName typ +/+ ".proto"

builtinGlobals :: [Definition]
builtinGlobals = map GlobalDefinition [
      declareFn "object_init" [intType, voidPtrType, voidPtrType] voidPtrType
    , declareFn "rc_malloc" [intType] voidPtrType
    , declareFn "rc_retain" [voidPtrType] VoidType
    , declareFn "rc_release" [voidPtrType] VoidType
    , declareFn "string_concat" [stringType, stringType] stringType
    , declareFn "array_create" [intType, intType] voidPtrType
    , declareFn "array_length" [voidPtrType] intType
    , declareFn "printInt" [intType] VoidType
    , declareFn "readInt" [] intType
    , declareFn "printString" [stringType] VoidType
    , declareFn "readString" [] stringType
    , declareFn "error" [] VoidType
    , defineFn "main" [] intType [BasicBlock (Name "entry")
        [UnName 1 := callDefaults (static $ composedName TObject +/+ "main") []]
        (Do $ Ret (Just $ LocalReference $ UnName 1) [])
      ]
  ]
  where
    declareFn str args ret = defineFn str args ret []
    defineFn str args ret blks = functionDefaults {
          name = Name str
        , parameters = ([Parameter typ (UnName 0) [] | typ <- args], False)
        , returnType = ret
        , basicBlocks = blks
      }

-- EMITTING CLASS HIERARCHY --
------------------------------
emitHierarchy :: EmitterEnv -> ClassHierarchy -> ErrorInfoT Identity [LlvmModule]
emitHierarchy env hierarchy = do
  layout <- lift $ layoutHierarchy hierarchy
  content <- runEmitterM (env { emitterenvLayout = layout }) $ do
    mapM_ define builtinGlobals
    Traversable.mapM emit hierarchy
  let modName = emitterenvModuleName env
  return [LlvmModule defaultModule {
          moduleName = modName
        , moduleDefinitions = content
      }]

-- EMITTER STATE --
-------------------
data EmitterState = EmitterState {
    emitterstateNextConst    :: Word
  , emitterstateNextId       :: Word
  , emitterstateBlockName    :: Maybe Name
  , emitterstateBlockBodyRev :: [Named Instruction]
  , emitterstateBlockRelease :: [Operand]
} deriving (Show)

emitterstateBlockBody :: EmitterState -> [Named Instruction]
emitterstateBlockBody = List.reverse . emitterstateBlockBodyRev

emitterstate0 :: EmitterState
emitterstate0 = EmitterState 0 0 Nothing [] []

-- EMITTER ENVIRONMENT --
-------------------------
data EmitterEnv = EmitterEnv {
    emitterenvModuleName :: String
  , emitterenvLayout     :: HierarchyLayout
} deriving (Show)

emitterenv0 :: EmitterEnv
emitterenv0 = EmitterEnv "unknown-module" undefined

-- THE MONAD --
---------------
type EmitterM a =
  WriterT [BasicBlock] (
    WriterT [Definition] (
      StateT EmitterState (
        ReaderT EmitterEnv (
          ErrorInfoT Identity)))) a
runEmitterM :: EmitterEnv -> EmitterM a -> ErrorInfoT Identity [Definition]
runEmitterM env action =
  runReaderT (evalStateT (execWriterT (runWriterT action)) emitterstate0) env

-- MONADIC HELPERS --
---------------------
notImplemented :: a
notImplemented = error "Not implemented"

assertM :: forall (m :: * -> *). Monad m => Bool -> m ()
assertM cond = assert cond $ return ()

lookupM :: forall r (m :: * -> *) k. (Monad m, Ord k) => k -> m (Map.Map k r) -> m r
lookupM key = liftM (Maybe.fromJust . Map.lookup key)

resetIds :: EmitterM ()
resetIds = modify $ \s -> s { emitterstateNextId = 0 }

nextId, nextConst :: EmitterM Word
nextId = do
  next <- gets emitterstateNextId
  modify $ \s -> s { emitterstateNextId = next + 1 }
  return next
nextConst = do
  next <- gets emitterstateNextConst
  modify $ \s -> s { emitterstateNextConst = next + 1 }
  return next

intercept :: forall (f :: * -> *) a a1. (Functor f, MonadWriter [a1] f) => f a -> f [a1]
intercept action = fmap snd $ censor (const []) $ listen action

layoutFor :: TypeComposed -> EmitterM ClassLayout
layoutFor typ = lookupM typ $ asks emitterenvLayout

-- LLVM MONADIC HELPERS --
--------------------------
nextVar, nextBlock, nextGlobal :: EmitterM Name
nextVar = Name <$> ('t':) <$> show <$> nextId
nextBlock = Name <$> ('b':) <$> show <$> nextId
nextGlobal = Name <$> ('c':) <$> show <$> nextConst

define :: Definition -> EmitterM ()
define = lift . tell . return

addInstr :: Named Instruction -> EmitterM ()
addInstr instr = modify $ \s -> s { emitterstateBlockBodyRev = instr : emitterstateBlockBodyRev s }

-- So sexy, singe llvm-general's constructors have proper arguments order (mostly) we can handle
-- many defaults and weird metadata fields this way.
class InstructionLike a b | a -> b where
  infix 6 |-
  (|-) :: (b -> Named b) -> a -> EmitterM ()
  infix 6 |=
  (|=) :: Name -> a -> EmitterM ()
  (|=) nam ins = (nam := ) |- ins

instance InstructionLike Instruction Instruction where
  (|-) nam ins = addInstr $ nam ins

instance InstructionLike (InstructionMetadata -> Instruction) Instruction where
  (|-) nam ins = addInstr $ nam (ins [])

instance InstructionLike (InstructionMetadata -> Terminator) Terminator where
  (|-) nam ins = endBlock $ nam (ins [])

newBlock :: Name  -> Name -> EmitterM a -> EmitterM a
newBlock bname back action = do
  oldname <- gets emitterstateBlockName
  assertM (Maybe.isNothing oldname)
  modify $ \s -> s { emitterstateBlockName = Just bname }
  res <- action
  endBlock $ Do $ Br back []
  return res

endBlock :: Named Terminator -> EmitterM ()
endBlock term = gets emitterstateBlockName >>= \x -> case x of
  Nothing -> return ()
  Just bname -> do
    releases <- gets emitterstateBlockRelease
    forM_ (List.reverse releases) $ \ref ->
      Do |- callDefaults (static $ Name "rc_release") [ref]
    body <- gets emitterstateBlockBody
    tell [BasicBlock bname body term]
    modify $ \s -> s {
        emitterstateBlockName = Nothing
      , emitterstateBlockBodyRev = []
      , emitterstateBlockRelease = []
    }

defaultValue :: TypeBasic -> Constant
defaultValue typ = case typ of
  TPrimitive TInt -> intValue 0
  TPrimitive TChar -> charValue '\0'
  TPrimitive TBool -> falseConst
  TComposed ctyp -> Null $ toLlvm ctyp
  _ -> Err.unreachable "no default value"

class Alignable a where
  align :: a -> Word32

instance Alignable TypeBasic where
  align (TComposed _) = alignRef
  align (TPrimitive _) = alignPrim

-- LLVM PURE HELPERS --
-----------------------
class Localizable a b | a -> b where
  location :: a -> b

instance Localizable VariableNum Name where
  location num = Name $ ("loc" ++) $ show $ fromEnum num

instance Localizable Variable Operand where
  location (Variable _ num _) = LocalReference $ location num

class Translatable a b | a -> b where
  toLlvm :: a -> b

instance Translatable Type LlvmType where
  toLlvm (TMethod typ) = toLlvm typ
  toLlvm (TBasic typ) = toLlvm typ

instance Translatable TypeMethod LlvmType where
  toLlvm (TypeMethod tret targs _) = FunctionType (toLlvm tret) (map toLlvm targs) False

instance Translatable TypeBasic LlvmType where
  toLlvm (TComposed typ) = toLlvm typ
  toLlvm (TPrimitive typ) = toLlvm typ

instance Translatable TypePrimitive LlvmType where
  toLlvm x = case x of
    TVoid -> VoidType
    TInt -> intType
    TChar -> charType
    TBool -> boolType

instance Translatable TypeComposed LlvmType where
  toLlvm x = case x of
    TArray typ -> PointerType (toLlvm typ) defAddrSpace
    TString -> stringType
    TNull -> Err.unreachable TNull
    _ -> PointerType (NamedTypeReference $ composedName x) defAddrSpace

instance Translatable ClassLayout LlvmType where
  toLlvm layout = StructureType {
        Llvm.Type.isPacked = False
      , elementTypes = map toLlvm $ layoutFieldTypes layout
    }

-- MEMORY MODEL --
------------------
-- We do reference counting for strings only (for now)
retain, release :: TypeBasic -> Operand -> EmitterM ()
retain (TComposed TString) ref@(LocalReference _) = do
  ref' <- castTo voidPtrType ref
  Do |- callDefaults (static $ Name "rc_retain") [ref']
retain _ _ = return ()
release (TComposed TString) ref@(LocalReference _) = do
  ref' <- castTo voidPtrType ref
  modify $ \s -> s { emitterstateBlockRelease = ref' : emitterstateBlockRelease s }
release _ _ = return ()

releaseAll :: [TypeBasic] -> [Operand] -> EmitterM ()
releaseAll types ops = mapM_ (uncurry release) (List.zip types ops)

sizeOf :: LlvmType -> Operand
sizeOf typ = let aNull = Null $ PointerType typ defAddrSpace in ConstantOperand $
  Llvm.Constant.PtrToInt (Llvm.Constant.GetElementPtr False aNull [intValue 1]) intType

-- TREE TRAVERSING --
---------------------
class Emitable a b | a -> b where
  emit :: a -> EmitterM b

instance Emitable Class () where
  emit clazz@(Class { classType = typ }) = do
    -- Emit class structure definition
    lay@(ClassLayout _ fields _) <- layoutFor typ
    let tdef = toLlvm lay
    define $ TypeDefinition (composedName typ) (Just tdef)
    -- Emit object prototype
    define $ GlobalDefinition $ globalVariableDefaults {
          name = objectProto typ
        , isConstant = True
        , Llvm.Global.type' = tdef
        , initializer = Just $ Struct False $ map defaultValue fields
      }
    -- Emit vtable
    -- FIXME
    -- Emit static methods
    mapM_ emit $ classAllMethods clazz

instance Emitable Method () where
  emit Method { methodBody = SInherited } = return ()
  emit Method { methodBody = SBuiltin } = return ()
  emit method@(Method {
        methodName = MethodName namestr
      , methodType = TypeMethod tret _ _
      , methodVariables = vars
      , methodOrigin = origin
      , methodBody = bodyStmt
      }) = do
    resetIds
    let args = if methodInstance method
        then argSelf : methodArgs method
        else methodArgs method
    let ltret = toLlvm tret
    largs <- mapM emitArg args
    blocks <- intercept $ do
      let entry = Name "entry"
      body <- nextBlock
      newBlock entry body $ do
        mapM_ allocVar $ args ++ vars
        mapM_ initArg args
        -- We have to initialize variables here for the first time for GC corectness
        mapM_ initVar vars
      exit <- nextBlock
      newBlock body exit $
        emit bodyStmt
      newBlock exit exit $
        Do |- if ltret == VoidType then Ret Nothing else Unreachable
    define $ GlobalDefinition $ functionDefaults {
        name = composedName origin +/+ namestr
      , returnType = ltret
      , parameters = (largs, False)
      , basicBlocks = blocks
    }
      where
        argSelf = Variable (TComposed origin) VariableThis (VariableName "self")
        argName num = Name $ ("arg" ++) $ show $ fromEnum num
        emitArg (Variable typ num _) = return $ Parameter (toLlvm typ) (argName num) []
        allocVar (Variable typ num _) = location num |= Alloca (toLlvm typ) Nothing (align typ)
        initArg var@(Variable typ num _) = let argRef = LocalReference $ argName num in do
          Do |- Store False (location var) argRef Nothing (align typ)
          -- Note that we are copying a reference when initializing argument
          retain typ argRef
        initVar var@(Variable typ _ _) =
          Do |- Store False (location var) (ConstantOperand $ defaultValue typ) Nothing (align typ)

instance Emitable (Operand, TypeComposed, FieldName) Operand where
  emit (op, ctyp, fname) = do
    ind <- lookupM fname $ layoutFieldOffsets <$> layoutFor ctyp
    res <- nextVar
    res |= Llvm.Instruction.GetElementPtr True op [
        ConstantOperand $ intValue 0
      , ConstantOperand $ intValue $ toInteger ind]
    populate res

instance Emitable Stmt () where
  emit x = case x of
    SEmpty -> return ()
    SBlock stmts -> mapM_ emit stmts
    SExpr rval typ -> do
      rop <- emit rval
      release typ rop
    -- Memory access
    SAssign lval rval typ -> do
      rop <- castTo typ =<< emit rval
      lop <- emit lval
      -- Firstly we have to fetch old value for reference counting
      tmp <- nextVar
      tmp |= Load False lop Nothing (align typ)
      release typ (LocalReference tmp)
      -- And then we can assign new one
      Do |- Store False lop rop Nothing (align typ)
    -- Control statements
    SReturn rval _ -> do
      rop <- emit rval
      Do |- Ret (Just rop)
    SReturnV -> Do |- Ret Nothing
    -- FIXME ref count
    SIf rval stmt -> undefined
    SIfElse rval stmt1 stmt2 -> undefined
    SWhile rval stmt -> undefined
    SThrow {} -> notImplemented
    STryCatch {} -> notImplemented
    -- Special function bodies
    SBuiltin -> Err.unreachable x
    SInherited -> Err.unreachable x
    -- Metainformation carriers
    SMetaLocation _ stmts -> mapM_ emit stmts
    -- These statements will be replaced with ones caring more context in subsequent phases
    PruneSDeclVar {} -> Err.unreachable x
    PruneSTryCatch {} -> Err.unreachable x

instance Emitable LValue Operand where
  -- We do not call retain/release for lvalues since they are local to instruction and cannot
  -- create (or remove) objects - all lvalue constructors refer to existing entities.
  emit x = case x of
    LVariable num _ -> populate $ location num
    LArrayElement lval rval _ -> do
      rop <- emit rval
      lop <- emit lval
      val <- nextVar
      val |= Load False lop Nothing alignRef
      res <- nextVar
      res |= Llvm.Instruction.GetElementPtr True (LocalReference val) [rop]
      populate res
    LField lval ctyp fname _ -> do
      lop <- emit lval
      val <- nextVar
      val |= Load False lop Nothing alignRef
      emit (LocalReference val, ctyp, fname)
    -- These expressions will be replaced with ones caring more context in subsequent phases
    PruneLExpr {} -> Err.unreachable x

instance Emitable RValue Operand where
  -- Objects might be created and destroyed while computing rvalue. Our invariants are:
  -- whenever a reference enters rvalue conputation we call retain on it unless it's a newly
  -- created object; whenever a  reference leaves computation (that means it is not propagated
  -- further) we call release.
  emit x = case x of
    -- FIXME ref count
    -- Literals
    ENull ->
      -- We deliberately skip retain here since it's a no-op, null can be treated as a new object
      -- to meet invariant assertion.
      populate $ Null (toLlvm TObject)
    ELitTrue -> populate trueConst
    ELitFalse -> populate falseConst
    ELitChar c -> populate $ charValue c
    ELitString str -> do
      -- This has to be kept in sync with struct rc_heaader definition in runtime library
      let val = Struct True [
              Llvm.Constant.Int 32 (-1)
            , Array charType $ map charValue str ++ [charValue '\0']]
      let len = fromIntegral (List.length str + 1) :: Word64
      cname <- nextGlobal
      define $ GlobalDefinition $ globalVariableDefaults {
            name = cname
          , isConstant = True
          , Llvm.Global.type' = StructureType True [
                IntegerType 32
              , ArrayType len charType]
          , initializer = Just val
        }
      let arr = Llvm.Constant.GetElementPtr True (GlobalReference cname) [intValue 0, intValue 1]
      -- There is no need to bump up a reference count since this is marked as constant
      populate $ Llvm.Constant.BitCast arr stringType
    ELitInt int -> populate $ intValue int
    -- Memory access
    ELoad num typ -> do
      res <- nextVar
      res |= Load False (LocalReference $ location num) Nothing (align typ)
      retain typ (LocalReference res)
      populate res
    EArrayLoad rval1 rval2 eltyp -> do
      rop2 <- emit rval2
      rop1 <- emit rval1
      loc <- nextVar
      loc |= Llvm.Instruction.GetElementPtr True rop1 [rop2]
      res <- nextVar
      res |= Load False (LocalReference loc) Nothing (align eltyp)
      retain eltyp (LocalReference res)
      release (TComposed $ TArray eltyp) rop1
      populate res
    EGetField rval ctyp fname ftyp -> do
      rop <- emit rval
      loc <- emit (rop, ctyp, fname)
      res <- nextVar
      res |= Load False loc Nothing (align ftyp)
      retain ftyp (LocalReference res)
      release (TComposed ctyp) rop
      populate res
    -- Method calls
    EInvokeStatic ctyp mname@(MethodName str) mtyp@(TypeMethod tret targs _) args -> do
      aops <- mapM emit args
      let nam = if ctyp == TNull || Builtins.isLibraryMethod ctyp mname mtyp
          then Name str
          else composedName ctyp +/+ str
      let callAndRelease dst = dst (callDefaults (static nam) aops) >> releaseAll targs aops
      if tret == (TPrimitive TVoid)
      then do
        callAndRelease (Do |-)
        populate $ Undef VoidType
      else do
        res <- nextVar
        callAndRelease (res |=)
        populate res
    EInvokeVirtual rval ctyp mname mtyp args -> undefined
    -- Object creation
    ENewObj ctyp -> do
      let PointerType lctyp _ = toLlvm ctyp
      let siz = sizeOf lctyp
      let proto = Llvm.Constant.BitCast (GlobalReference $ objectProto ctyp) voidPtrType
      ptr <- nextVar
      ptr |= callDefaults (static $ Name "rc_malloc") [siz]
      Do |- callDefaults (static $ Name "object_init")
          [siz, (LocalReference ptr), (ConstantOperand proto)]
      obj <- nextVar
      obj |= Llvm.Instruction.BitCast (LocalReference ptr) (toLlvm ctyp)
      populate obj
    ENewArr eltyp rval -> do
      let siz = sizeOf $ toLlvm eltyp
      rop <- emit rval
      ptr <- nextVar
      ptr |= callDefaults (static $ Name "array_create") [rop, siz]
      obj <- nextVar
      obj |= Llvm.Instruction.BitCast (LocalReference ptr) (PointerType (toLlvm eltyp) defAddrSpace)
      populate obj
    -- Operations
    EUnary _ _ (TPrimitive TBool) -> evalCond x
    EUnary OuNeg expr tret -> undefined
    EUnary {} -> Err.unreachable x
    EBinary _ _ _ (TPrimitive TBool) -> evalCond x
    EBinary ObPlus expr1 expr2 (TComposed TString) ->
      let strType = TComposed TString
          concatType = TypeMethod strType [strType, strType] [] in do
        emit $ EInvokeStatic TNull (MethodName "string_concat") concatType [expr1, expr2]
    EBinary opbin expr1 expr2 (TPrimitive TInt) -> do
      eop1 <- emit expr1
      eop2 <- emit expr2
      res <- nextVar
      case opbin of
        ObTimes -> res |= Llvm.Instruction.Mul False False eop1 eop2
        ObDiv -> res |= Llvm.Instruction.SDiv True eop1 eop2
        ObMod -> res |= Llvm.Instruction.SRem eop1 eop2
        ObPlus -> res |= Llvm.Instruction.Add False False eop1 eop2
        ObMinus -> res |= Llvm.Instruction.Sub False False eop1 eop2
        _ -> Err.unreachable "should be handled in TPrimitive TBool branch"
      populate res
    EBinary {} -> Err.unreachable x
    -- These expressions will be replaced with ones caring more context in subsequent phases
    PruneEVar {} -> Err.unreachable x

-- JUMPING CODE --
------------------
class EmitableConditional a where
  emitCond :: a -> Name -> Name -> EmitterM ()
  evalCond :: a -> EmitterM Operand
  evalCond x = undefined

instance EmitableConditional RValue where
  emitCond = undefined

-- TRAVERSING HELPERS --
------------------------
class Populable a where
  populate :: a -> EmitterM Operand

instance Populable Name where
  populate = return . LocalReference

instance Populable Constant where
  populate = return . ConstantOperand

class CastingTarget a where
  castTo :: a -> Operand -> EmitterM Operand

instance CastingTarget TypeBasic where
  castTo = castTo . toLlvm

instance CastingTarget LlvmType where
  castTo typ op = do
    tmp <- nextVar
    tmp |= Llvm.Instruction.BitCast op typ
    populate tmp


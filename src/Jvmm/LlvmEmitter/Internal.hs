{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
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
import qualified Data.Ord as Ord
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
import LLVM.General.AST.Type as Llvm.Type hiding (Type)
import qualified LLVM.General.AST.Type as Llvm.Type (Type)

type LlvmType = Llvm.Type.Type

-- LLVM RUNTIME AND BUILTINS --
-------------------------------
-- TODO clean this out
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

castToVoidPtr :: Operand -> EmitterM Operand
castToVoidPtr op = do
  tmp <- nextVar
  tmp |= Llvm.Instruction.BitCast op voidPtrType
  return $ LocalReference tmp

castToVoidPtr' :: Constant -> Constant
castToVoidPtr' c = Llvm.Constant.BitCast c voidPtrType

intValue :: Integer -> Constant
intValue = Llvm.Constant.Int 32 . fromIntegral
intValue' :: Int -> Operand
intValue' = ConstantOperand . Llvm.Constant.Int 32 . fromIntegral
charValue :: Char -> Constant
charValue int = Llvm.Constant.Int 8 $ toInteger $ Char.ord int

trueConst, falseConst :: Constant
trueConst = Llvm.Constant.Int 1 1
falseConst = Llvm.Constant.Int 1 0

alignRef, alignPrim, align0 :: Word32
alignRef = 0 -- 8
alignPrim = 0 -- 4
align0 = 0

entryBlockName, exitBlockName, returnLoc :: Name
entryBlockName = Name "entry"
exitBlockName = Name "exit"
returnLoc = Name "ret"

composedName :: TypeComposed -> Name
composedName x = case x of
  TObject -> Name "class.Object"
  TUser (ClassName str) -> Name $ "class." ++ str
  _ -> Err.unreachable "type has no name"

(+/+) :: Name -> String -> Name
(+/+) (Name pref) suf = Name $ pref ++ "." ++ suf
(+/+) x@(UnName _) _ = x

memberMethod :: TypeComposed -> TypeMethod -> TypeMethod
memberMethod ctyp mtyp@(TypeMethod _ args _) =
  mtyp { typemethodArgs = TComposed ctyp : args }

objectProto, objectVtable :: TypeComposed -> Name
objectProto typ = composedName typ +/+ ".proto"
objectVtable typ = composedName typ +/+ ".vtable"

builtinGlobals :: [Definition]
builtinGlobals = map GlobalDefinition [
      declareFn "object_init" [intType, voidPtrType, voidPtrType] voidPtrType
    , declareFn "rc_malloc" [intType] voidPtrType
    , declareFn "rc_retain" [voidPtrType] VoidType
    , declareFn "rc_release" [voidPtrType] VoidType
    , declareFn "string_concat" [stringType, stringType] stringType
    , declareFn "string_length" [stringType] intType
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

class Localizable a b | a -> b where
  location :: a -> b

instance Localizable VariableNum Name where
  location num = Name $ ("var" ++) $ show $ fromEnum num

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
    _ -> PointerType (NamedTypeReference $ composedName x) defAddrSpace

instance Translatable ClassLayout (LlvmType -> LlvmType) where
  toLlvm layout vtable = StructureType {
        Llvm.Type.isPacked = False
      , elementTypes = PointerType vtable defAddrSpace : map toLlvm (layoutFieldTypes layout)
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

assertM :: Monad m => Bool -> m ()
assertM cond = assert cond $ return ()

lookupM :: (Monad m, Ord k) => k -> m (Map.Map k r) -> m r
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

intercept :: (Functor f, MonadWriter [a1] f) => f a -> f [a1]
intercept action = fmap snd $ censor (const []) $ listen action

layoutFor :: TypeComposed -> EmitterM ClassLayout
layoutFor typ = lookupM typ $ asks emitterenvLayout

-- NAMES ALLOCATION --
----------------------
nextVar, nextBlock, nextGlobal :: EmitterM Name
nextVar = Name <$> ('t':) <$> show <$> nextId
nextBlock = Name <$> ('b':) <$> show <$> nextId
nextGlobal = Name <$> ('c':) <$> show <$> nextConst

nextBlocks2 :: EmitterM (Name, Name)
nextBlocks2 = liftM2 (,) nextBlock nextBlock
nextBlocks3 :: EmitterM (Name, Name, Name)
nextBlocks3 = liftM3 (,,) nextBlock nextBlock nextBlock

-- GLOBALS DEFINITIONS --
-------------------------
define :: Definition -> EmitterM ()
define = lift . tell . return

withLocalVar :: (Name -> EmitterM a) -> EmitterM Operand
withLocalVar action = do
  res <- nextVar
  action res
  return $ LocalReference res

withConstant :: Constant -> EmitterM Operand
withConstant = return . ConstantOperand

-- BASIC BLOCKS --
------------------
-- So sexy, since llvm-general's constructors have proper arguments order (mostly) we can handle
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

noOp :: EmitterM ()
noOp = return ()

addInstr :: Named Instruction -> EmitterM ()
addInstr instr = modify $ \s -> s { emitterstateBlockBodyRev = instr : emitterstateBlockBodyRev s }

undefBlock :: Name
undefBlock = error "block to jump to is not defined"

newBlock :: Name  -> Name -> EmitterM a -> EmitterM a
newBlock bname back action = do
  divideBlock bname
  res <- action
  endBlock $ Do $ Br back []
  return res

divideBlock :: Name -> EmitterM ()
divideBlock bname = do
  oldname <- gets emitterstateBlockName
  assertM (Maybe.isNothing oldname)
  modify $ \s -> s { emitterstateBlockName = Just bname }

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

-- MEMORY MODEL --
------------------
-- We do reference counting for strings only (for now)
whenRefCounted :: TypeBasic -> EmitterM () -> EmitterM ()
whenRefCounted (TComposed TString) = id
whenRefCounted _ =  const $ return ()

class ReferenceCounted a where
  retain, release :: a -> EmitterM ()

instance ReferenceCounted (TypeBasic, Operand) where
  retain (typ, ref@(LocalReference _)) = whenRefCounted typ $ do
    ref' <- castToVoidPtr ref
    Do |- callDefaults (static $ Name "rc_retain") [ref']
  retain _ = return ()
  release (typ, ref@(LocalReference _)) = whenRefCounted typ $ do
    ref' <- castToVoidPtr ref
    -- TODO do this lazily at the very end of each block
    Do |- callDefaults (static $ Name "rc_release") [ref']
    --modify $ \s -> s { emitterstateBlockRelease = ref' : emitterstateBlockRelease s }
  release _ = return ()

instance ReferenceCounted (TypeComposed, Operand) where
  retain (typ, op) = retain (TComposed typ, op)
  release (typ, op) = retain (TComposed typ, op)

instance ReferenceCounted (TypeBasic, Name) where
  retain (typ, nam) = retain (typ, LocalReference nam)
  release (typ, nam) = release (typ, LocalReference nam)

instance ReferenceCounted Variable where
  retain var@(Variable typ _ _) = whenRefCounted typ $ do
    tmp <- nextVar
    tmp |= Load False (location var) Nothing (align typ)
    retain (typ, tmp)
  release var@(Variable typ _ _) = whenRefCounted typ $ do
    tmp <- nextVar
    tmp |= Load False (location var) Nothing (align typ)
    release (typ, tmp)

retain', release' :: ReferenceCounted (a, b) => a -> b -> EmitterM ()
retain' = curry retain
release' = curry release

sizeOf :: LlvmType -> Operand
sizeOf typ = let aNull = Null $ PointerType typ defAddrSpace in ConstantOperand $
  Llvm.Constant.PtrToInt (Llvm.Constant.GetElementPtr False aNull [intValue 1]) intType

-- TREE TRAVERSING --
---------------------
class Emitable a b | a -> b where
  emit :: a -> EmitterM b

instance Emitable Class () where
  emit clazz@(Class { classType = typ }) = do
    lay@(ClassLayout _ fields methods) <- layoutFor typ
    -- Emit vtable
    let vtableMethods = map snd $ List.sortBy (Ord.comparing fst)
          [(ind, GlobalReference (composedName origin +/+ namestr)) |
              Method { methodOrigin = origin, methodName = name' } <- classInstanceMethods clazz
            , (MethodName namestr, ind) <- Map.toList methods
            , MethodName namestr == name']
    let vtableType = ArrayType (fromIntegral $ length vtableMethods) voidPtrType
    let vtableValue = Array voidPtrType $ map castToVoidPtr' vtableMethods
    define $ GlobalDefinition $ globalVariableDefaults {
          name = objectVtable typ
        , isConstant = True
        , Llvm.Global.type' = vtableType
        , initializer = Just vtableValue
      }
    -- Emit class structure definition
    let classDef = toLlvm lay vtableType
    define $ TypeDefinition (composedName typ) (Just classDef)
    -- Emit object prototype
    let classProto = Struct False $ GlobalReference (objectVtable typ) : map defaultValue fields
    define $ GlobalDefinition $ globalVariableDefaults {
          name = objectProto typ
        , isConstant = True
        , Llvm.Global.type' = classDef
        , initializer = Just classProto
      }
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
      body <- nextBlock
      newBlock entryBlockName body $ do
        mapM_ allocVar $ args ++ vars
        mapM_ initArg args
        -- We have to initialize variables here for the first time for GC corectness
        mapM_ initVar vars
        when (ltret /= VoidType) $
          -- Allocate place for return value
          returnLoc |= Alloca ltret Nothing (align tret)
      newBlock body exitBlockName $
        emit bodyStmt
      newBlock exitBlockName undefBlock $ do
        mapM_ release args
        mapM_ release vars
        if ltret == VoidType
        then Do |- Ret Nothing
        else do
          tmp <- nextVar
          tmp |= Load False (LocalReference returnLoc) Nothing (align tret)
          Do |- Ret (Just (LocalReference tmp))
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
          retain' typ argRef
        initVar var@(Variable typ _ _) =
          Do |- Store False (location var) (ConstantOperand $ defaultValue typ) Nothing (align typ)

instance Emitable (Operand, TypeComposed, FieldName) Operand where
  emit (op, ctyp, fname) = do
    ind <- lookupM fname $ layoutFieldOffsets <$> layoutFor ctyp
    emit (op, ind)

instance Emitable (Operand, Int) Operand where
  emit (op, ind) = withLocalVar $ \res -> do
    res |= Llvm.Instruction.GetElementPtr True op [intValue' 0, intValue' ind]

instance Emitable Stmt () where
  emit x = case x of
    SEmpty -> return ()
    SBlock stmts -> mapM_ emit stmts
    SExpr rval typ -> do
      rop <- emit rval
      release' typ rop
    -- Memory access
    SAssign lval rval typ -> do
      rop <- emit rval
      lop <- emit lval
      whenRefCounted typ $ do
        -- Firstly we have to fetch old value for reference counting
        tmp <- nextVar
        tmp |= Load False lop Nothing (align typ)
        release' typ tmp
      -- And then we can assign new one
      Do |- Store False lop rop Nothing (align typ)
    -- Control statements
    SReturn rval typ -> do
      rop <- emit rval
      Do |- Store False (LocalReference returnLoc) rop Nothing (align typ)
      Do |- Br exitBlockName
    SReturnV -> Do |- Br exitBlockName
    SIf rval stmt -> do
      (btrue, bcont) <- nextBlocks2
      emitCond rval btrue bcont
      newBlock btrue bcont $ emit stmt
      divideBlock bcont
    SIfElse rval stmt1 stmt2 -> do
      (btrue, bfalse, bcont) <- nextBlocks3
      emitCond rval btrue bfalse
      newBlock btrue bcont $ emit stmt1
      newBlock bfalse bcont $ emit stmt2
      divideBlock bcont
    SWhile rval stmt -> do
      (bloop, bcond, bcont) <- nextBlocks3
      Do |- Br bcond
      newBlock bloop bcond $ emit stmt
      newBlock bcond undefBlock $ emitCond rval bloop bcont
      divideBlock bcont
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
    LVariable num _ -> return $ LocalReference $ location num
    LArrayElement lval rval _ -> withLocalVar $ \res -> do
      rop <- emit rval
      val <- loadLValue lval
      res |= Llvm.Instruction.GetElementPtr True val [rop]
    LField lval ctyp fname _ -> do
      val <- loadLValue lval
      emit (val, ctyp, fname)
    -- These expressions will be replaced with ones caring more context in subsequent phases
    PruneLExpr {} -> Err.unreachable x
    where
      loadLValue lval = do
        lop <- emit lval
        val <- nextVar
        val |= Load False lop Nothing alignRef
        return (LocalReference val)

instance Emitable RValue Operand where
  -- Objects might be created and destroyed while computing rvalue. Our invariants are:
  -- whenever a reference enters rvalue conputation we call retain on it unless it's a newly
  -- created object; whenever a  reference leaves computation (that means it is not propagated
  -- further) we call release.
  emit x = case x of
    -- Literals
    ENull typ ->
      -- We deliberately skip retain here since it's a no-op, null can be treated as a new object
      -- to meet invariant assertion.
      withConstant $ Null (toLlvm typ)
    ELitTrue -> withConstant trueConst
    ELitFalse -> withConstant falseConst
    ELitChar c -> withConstant $ charValue c
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
      withConstant $ Llvm.Constant.BitCast arr stringType
    ELitInt int -> withConstant $ intValue int
    -- Memory access
    ELoad num typ -> withLocalVar $ \res -> do
      res |= Load False (LocalReference $ location num) Nothing (align typ)
      retain' typ res
    EArrayLoad rval1 rval2 eltyp -> withLocalVar $ \res -> do
      rop2 <- emit rval2
      rop1 <- emit rval1
      loc <- nextVar
      loc |= Llvm.Instruction.GetElementPtr True rop1 [rop2]
      res |= Load False (LocalReference loc) Nothing (align eltyp)
      retain' eltyp res
      release' (TComposed $ TArray eltyp) rop1
    -- FIXME
    EGetField rval TString (FieldName "length") ftyp -> do
      undefined
    -- FIXME
    EGetField rval ctyp@(TArray _) (FieldName "length") _ -> withLocalVar $ \res -> do
      rop <- emit rval
      tmp <- nextVar
      tmp |= Llvm.Instruction.BitCast rop voidPtrType
      res |= callDefaults (static $ Name "array_length") [LocalReference tmp]
      release' ctyp rop
    EGetField rval ctyp fname ftyp -> withLocalVar $ \res -> do
      rop <- emit rval
      loc <- emit (rop, ctyp, fname)
      res |= Load False loc Nothing (align ftyp)
      retain' ftyp res
      release' (TComposed ctyp) rop
    -- Method calls
    EInvokeStatic ctyp mname@(MethodName str) mtyp@(TypeMethod tret targs _) args -> do
      aops <- mapM emit args
      let nam = if Builtins.isLibraryMethod ctyp mname mtyp
          then Name str
          else composedName ctyp +/+ str
      let callAndRelease dst = do
          dst (callDefaults (static nam) aops)
          mapM_ release $ List.zip targs aops
      if tret == TPrimitive TVoid
      then do
        callAndRelease (Do |-)
        withConstant $ Undef VoidType
      else withLocalVar $ \res -> callAndRelease (res |=)
    -- FIXME
    EInvokeVirtual rval TString (MethodName "charAt") mtyp [arg1] -> do
      undefined
    EInvokeVirtual rval ctyp mname mtyp' args -> do
      let mtyp@(TypeMethod tret targs _) = memberMethod ctyp mtyp'
      rop <- emit rval
      -- Get vtab address
      vloc <- emit (rop, 0 :: Int)
      vtab <- nextVar
      vtab |= Load False vloc Nothing alignRef
      mloc <- nextVar
      -- Note that we must be within bounds since presence of a method is determined statically
      ind <- lookupM mname $ layoutMethodOffsets <$> layoutFor ctyp
      mloc |= Llvm.Instruction.GetElementPtr True (LocalReference vtab) [intValue' 0, intValue' ind]
      mptr <- nextVar
      mptr |= Load False (LocalReference mloc) Nothing alignRef
      fun <- nextVar
      fun |= Llvm.Instruction.BitCast (LocalReference mptr) (PointerType (toLlvm mtyp) defAddrSpace)
      -- We no have function address
      aops <- (rop:) <$> mapM emit args
      let callAndRelease dst = do
          dst (callDefaults (dynamic fun) aops)
          mapM_ release $ List.zip targs aops
      if tret == TPrimitive TVoid
      then do
        callAndRelease (Do |-)
        withConstant $ Undef VoidType
      else withLocalVar $ \res -> callAndRelease (res |=)
    -- Object creation
    ENewObj ctyp -> withLocalVar $ \res -> do
      let PointerType lctyp _ = toLlvm ctyp
      let siz = sizeOf lctyp
      let proto = Llvm.Constant.BitCast (GlobalReference $ objectProto ctyp) voidPtrType
      ptr <- nextVar
      ptr |= callDefaults (static $ Name "rc_malloc") [siz]
      Do |- callDefaults (static $ Name "object_init")
          [siz, LocalReference ptr, ConstantOperand proto]
      res |= Llvm.Instruction.BitCast (LocalReference ptr) (toLlvm ctyp)
    ENewArr eltyp rval -> withLocalVar $ \res -> do
      let leltyp = toLlvm eltyp
      let siz = sizeOf leltyp
      rop <- emit rval
      ptr <- nextVar
      ptr |= callDefaults (static $ Name "array_create") [rop, siz]
      res |= Llvm.Instruction.BitCast (LocalReference ptr) (PointerType leltyp defAddrSpace)
    -- Operations
    EUnary OuNot expr (TPrimitive TBool) -> withLocalVar $ \res -> do
      eop <- emit expr
      res |= Llvm.Instruction.Xor (ConstantOperand falseConst) eop
    EUnary OuNeg expr typ@(TPrimitive TInt) -> emit $ EBinary ObMinus (ELitInt 0) expr typ typ typ
    EUnary {} -> Err.unreachable x
    EBinary ObAnd _ _ _ _ _ -> withLocalVar $ \res -> evalCond x res
    EBinary ObOr _ _ _ _ _ -> withLocalVar $ \res -> evalCond x res
    EBinary opbin expr1 expr2 (TPrimitive TBool) typ1 typ2 -> withLocalVar $ \res -> do
      eop1 <- emit expr1
      eop2 <- emit expr2
      let p = case opbin of
            ObLTH -> SLT
            ObLEQ -> SLE
            ObGTH -> SGT
            ObGEQ -> SGE
            ObEQU -> Llvm.IntegerPrediacte.EQ
            ObNEQ -> NE
            _ -> Err.unreachable "unknown operation"
      res |= Llvm.Instruction.ICmp p eop1 eop2
      release' typ1 eop1
      release' typ2 eop2
    EBinary ObPlus expr1 expr2 (TComposed TString) _ _ -> withLocalVar $ \res -> do
      eop1 <- emit expr1
      eop2 <- emit expr2
      res |= callDefaults (static $ Name "string_concat") [eop1, eop2]
      release' (TComposed TString) eop1
      release' (TComposed TString) eop2
    EBinary opbin expr1 expr2 (TPrimitive TInt) _ _ -> withLocalVar $ \res -> do
      eop1 <- emit expr1
      eop2 <- emit expr2
      case opbin of
        ObTimes -> res |= Llvm.Instruction.Mul False False eop1 eop2
        ObDiv -> res |= Llvm.Instruction.SDiv True eop1 eop2
        ObMod -> res |= Llvm.Instruction.SRem eop1 eop2
        ObPlus -> res |= Llvm.Instruction.Add False False eop1 eop2
        ObMinus -> res |= Llvm.Instruction.Sub False False eop1 eop2
        _ -> Err.unreachable "unknown operation"
    EBinary {} -> Err.unreachable x
    -- These expressions will be replaced with ones caring more context in subsequent phases
    PruneEVar {} -> Err.unreachable x
    PruneENull {} -> Err.unreachable x

-- JUMPING CODE --
------------------
class EmitableConditional a where
  emitCond :: a -> Name -> Name -> EmitterM ()
  evalCond :: a -> Name -> EmitterM ()
  evalCond x res = do
    (btrue, bfalse, bcont) <- nextBlocks3
    emitCond x btrue bfalse
    newBlock btrue bcont noOp
    newBlock bfalse bcont noOp
    divideBlock bcont
    res |= Phi boolType [(ConstantOperand trueConst, btrue), (ConstantOperand falseConst, bfalse)]

instance EmitableConditional RValue where
  emitCond x btrue bfalse = case x of
    -- Literals
    ELitTrue -> Do |- Br btrue
    ELitFalse -> Do |- Br bfalse
    -- Memory access
    ELoad _ (TPrimitive TBool) -> emitAndJump
    EArrayLoad _ _ (TPrimitive TBool) -> emitAndJump
    EGetField {} -> emitAndJump
    -- Method calls
    EInvokeStatic _ (MethodName _) (TypeMethod (TPrimitive TBool) _ _) _ -> emitAndJump
    EInvokeVirtual {} -> emitAndJump
    -- Object creation
    -- Operations
    EUnary OuNot expr _ -> emitCond expr bfalse btrue
    EBinary ObAnd expr1 expr2 _ _ _ -> do
      bmid <- nextBlock
      emitCond expr1 bmid bfalse
      newBlock bmid undefBlock $ emitCond expr2 btrue bfalse
    EBinary ObOr expr1 expr2 _ _ _ -> do
      bmid <- nextBlock
      emitCond expr1 btrue bmid
      newBlock bmid undefBlock $ emitCond expr2 btrue bfalse
    EBinary {} -> emitAndJump
    -- If we cannot recognize expression as binary one
    _ -> badExpressionType
    where
      emitAndJump = do
        res <- emit x
        Do |- CondBr res btrue bfalse
      badExpressionType = Err.unreachable $ "attempt to emit jump code for non-boolean" ++ show x


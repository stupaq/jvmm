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

import Text.Show.Pretty (ppShow)

type LlvmType = Llvm.Type.Type

-- BUILTINS AND RUNTIME --
--------------------------
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

intValue, charValue, boolValue :: Integral a => a -> Constant
intValue int = Llvm.Constant.Int 32 $ toInteger int
charValue int = Llvm.Constant.Int 8 $ toInteger int
boolValue int = Llvm.Constant.Int 1 $ toInteger int

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
      declareFn "rc_malloc" [intType] voidPtrType
    , declareFn "rc_retain" [voidPtrType] VoidType
    , declareFn "rc_release" [voidPtrType] VoidType
    , declareFn "string_concat" [stringType, stringType] stringType
    , declareFn "array_malloc" [intType, intType] voidPtrType
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
  -- FIXME
  --error $ ppShow content
  let modName = emitterenvModuleName env
  return [LlvmModule defaultModule {
          moduleName = modName
        , moduleDefinitions = content
      }]

-- EMITTER STATE --
-------------------
data EmitterState = EmitterState {
    emitterstateNextId       :: Word
  , emitterstateBlockName    :: Maybe Name
  , emitterstateBlockBody    :: [Named Instruction]
  , emitterstateBlockRelease :: [Operand]
} deriving (Show)

emitterstate0 :: EmitterState
emitterstate0 = EmitterState 0 Nothing [] []

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

nextId :: EmitterM Word
nextId = do
  next <- gets emitterstateNextId
  modify $ \s -> s { emitterstateNextId = next + 1 }
  return next

intercept :: forall (f :: * -> *) a a1. (Functor f, MonadWriter [a1] f) => f a -> f [a1]
intercept action = fmap snd $ censor (const []) $ listen action

layoutFor :: TypeComposed -> EmitterM ClassLayout
layoutFor typ = lookupM typ $ asks emitterenvLayout

-- LLVM MONADIC HELPERS --
--------------------------
nextVar :: EmitterM Name
nextVar = Name <$> ('t':) <$> show <$> nextId

nextBlock :: EmitterM Name
nextBlock = Name <$> ('b':) <$> show <$> nextId

define :: Definition -> EmitterM ()
define = lift . tell . return

addInstr :: Named Instruction -> EmitterM ()
addInstr instr = modify $ \s -> s { emitterstateBlockBody = instr : emitterstateBlockBody s }

-- So sexy, singe llvm-general's constructors have proper argument order (mostly) we can handle
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
endBlock term = get >>= \x -> case x of
    EmitterState _ Nothing _ _ -> return ()
    EmitterState _ (Just bname) rbody _ -> do
      releases <- gets emitterstateBlockRelease
      forM_ (List.reverse releases) $ \ref ->
        Do |- callDefaults (static $ Name "rc_retain") [ref]
      tell [BasicBlock bname (List.reverse rbody) term]
      modify $ \s -> s {
          emitterstateBlockName = Nothing
        , emitterstateBlockBody = []
        , emitterstateBlockRelease = []
      }

defaultValue :: TypeBasic -> Constant
defaultValue typ = case typ of
  TPrimitive TInt -> intValue (0 :: Int)
  TPrimitive TChar -> charValue (0 :: Int)
  TPrimitive TBool -> boolValue (0 :: Int)
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
retain, release :: TypeBasic -> Operand -> EmitterM ()
retain (TComposed _) ref@(LocalReference _) =
  modify $ \s -> s { emitterstateBlockRelease = ref : emitterstateBlockRelease s }
retain _ _ = return ()
release (TComposed _) ref@(LocalReference _) =
  Do |- callDefaults (static $ Name "rc_release") [ref]
release _ _ = return ()

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
      exit <- nextBlock
      newBlock body exit $
        emit bodyStmt
      newBlock exit exit $
        if ltret == VoidType
        then Do |- Ret Nothing
        else Do |- Unreachable
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
        initArg var@(Variable typ num _) =
          Do |- Store False (location var) (LocalReference $ argName num) Nothing (align typ)

instance Emitable Stmt () where
  emit x = case x of
    SEmpty -> return ()
    SBlock stmts -> mapM_ emit stmts
    SExpr rval typ -> do
      rop <- emit rval
      release typ rop
    -- Memory access
    SAssign lval rval typ -> do
      lop <- emit lval
      rop <- emit rval
      Do |- Store False lop rop Nothing (align typ)
    -- Control statements
    SReturn rval _ -> do
      rop <- emit rval
      Do |- Ret (Just rop)
    SReturnV -> Do |- Ret Nothing
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
  -- FIXME ref count
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
      res <- nextVar
      ind <- lookupM fname $ layoutFieldOffsets <$> layoutFor ctyp
      res |= Llvm.Instruction.GetElementPtr True (LocalReference val) [
            ConstantOperand $ intValue (0 :: Int)
          , ConstantOperand $ intValue ind]
      populate res
    -- These expressions will be replaced with ones caring more context in subsequent phases
    PruneLExpr {} -> Err.unreachable x

instance Emitable RValue Operand where
  -- FIXME ref count
  emit x = case x of
    -- FIXME
    -- Literals
    ENull -> populate $ Null (toLlvm TObject)
    ELitTrue -> populate $ boolValue (1 :: Int)
    ELitFalse -> populate $ boolValue (0 :: Int)
    ELitChar c -> populate $ charValue (Char.ord c)
    ELitString str -> undefined
    ELitInt int -> populate $ intValue int
    -- Memory access
    ELoad num typ -> do
      res <- nextVar
      res |= Load False (LocalReference $ location num) Nothing (align typ)
      populate res
    EArrayLoad rval1 rval2 eltyp -> do
      rop2 <- emit rval2
      rop1 <- emit rval1
      loc <- nextVar
      loc |= Llvm.Instruction.GetElementPtr True rop1 [rop2]
      res <- nextVar
      res |= Load False (LocalReference $ loc) Nothing (align eltyp)
      populate res
    EGetField rval ctyp fname ftyp -> do
      rop <- emit rval
      loc <- nextVar
      ind <- lookupM fname $ layoutFieldOffsets <$> layoutFor ctyp
      loc |= Llvm.Instruction.GetElementPtr True rop [
            ConstantOperand $ intValue (0 :: Int)
          , ConstantOperand $ intValue ind]
      res <- nextVar
      res |= Load False (LocalReference $ loc) Nothing (align ftyp)
      populate res
    -- Method calls
    EInvokeStatic ctyp mname mtyp args -> undefined
    EInvokeVirtual rval ctyp mname mtyp args -> undefined
    -- Object creation
    ENewObj ctyp -> undefined
    ENewArr eltyp rval -> undefined
    -- Operations
    EUnary opun rval typ -> undefined
    EBinary opbin rval1 rval2 typ -> undefined
    -- These expressions will be replaced with ones caring more context in subsequent phases
    PruneEVar {} -> Err.unreachable x

-- TRAVERSING HELPERS --
------------------------
class Populable a where
  populate :: a -> EmitterM Operand

instance Populable Name where
  populate = return . LocalReference

instance Populable Constant where
  populate = return . ConstantOperand


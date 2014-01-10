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
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

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
import LLVM.General.AST.Name as Llvm.Name
import LLVM.General.AST.Operand as Llvm.Operand
import LLVM.General.AST.RMWOperation as Llvm.RMWOperation
import LLVM.General.AST.Type as Llvm.Type hiding (Type)
import qualified LLVM.General.AST.Type as Llvm.Type (Type)

import Text.Show.Pretty (ppShow)

type LlvmType = Llvm.Type.Type

-- BUILTINS AND RUNTIME --
--------------------------
defCall :: Name -> [Operand] -> Instruction
defCall fname args = Call {
      isTailCall = False
    , Llvm.Instruction.callingConvention = Llvm.CallingConvention.C
    , Llvm.Instruction.returnAttributes = []
    , function =  Right $ ConstantOperand $ GlobalReference fname
    , arguments = [(op, []) | op <- args]
    , Llvm.Instruction.functionAttributes = []
    , metadata = []
  }

defAddrSpace :: AddrSpace
defAddrSpace = AddrSpace 0

boolType, intType, charType, voidPtrType, stringType :: LlvmType
boolType = IntegerType 1
intType = IntegerType 32
charType = IntegerType 8
voidPtrType = PointerType charType defAddrSpace
stringType = PointerType charType defAddrSpace

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
    , defineFn "main" [] intType [BasicBlock
        (UnName 0)
        [UnName 1 := defCall (Name "class.Object.main") []]
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
assertM :: forall (m :: * -> *). Monad m => Bool -> m ()
assertM cond = assert cond $ return ()

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
layoutFor typ = asks emitterenvLayout >>= return . Maybe.fromJust . Map.lookup typ

-- LLVM MONADIC HELPERS --
--------------------------
nextUnName :: EmitterM Name
nextUnName = UnName <$> nextId

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

instance InstructionLike (InstructionMetadata -> Instruction) Instruction where
  (|-) nam ins = addInstr $ nam (ins [])

instance InstructionLike (InstructionMetadata -> Terminator) Terminator where
  (|-) nam ins = endBlock $ nam (ins [])

newBlock :: Name  -> Name -> EmitterM a -> EmitterM a
newBlock bname back action = do
  oldname <- gets emitterstateBlockName
  assertM (oldname == Nothing)
  modify $ \s -> s { emitterstateBlockName = Just bname }
  res <- action
  endBlock $ Do $ Br back []
  return res

endBlock :: Named Terminator -> EmitterM ()
endBlock term = get >>= \x -> case x of
    EmitterState _ Nothing _ _ -> return ()
    EmitterState _ (Just bname) rbody _ -> do
      -- TODO handle releases
      tell [BasicBlock bname (List.reverse rbody) term]
      modify $ \s -> s {
          emitterstateBlockName = Nothing
        , emitterstateBlockBody = []
        , emitterstateBlockRelease = []
      }

defaultValue :: TypeBasic -> EmitterM Constant
defaultValue typ = case typ of
  TPrimitive TInt -> return $ Llvm.Constant.Int 32 0
  TPrimitive TChar -> return $ Llvm.Constant.Int 8 0
  TPrimitive TBool -> return $ Llvm.Constant.Int 1 0
  TComposed ctyp -> Null <$> emit ctyp
  _ -> Err.unreachable "no default value"

-- FIXME/
-- TREE TRAVERSING --
---------------------
class Emitable a b | a -> b where
  emit :: a -> EmitterM b

instance Emitable Class () where
  emit clazz@(Class { classType = typ }) = do
    -- Emit class structure definition
    NamedTypeReference tname <- emit typ
    tdef <- emit =<< layoutFor typ
    define $ TypeDefinition tname (Just tdef)
    -- Emit static methods
    mapM_ emit $ classStaticMethods clazz

instance Emitable ClassLayout LlvmType where
  emit layout = do
    fields <- mapM emit $ layoutFieldTypes layout
    return StructureType {
        Llvm.Type.isPacked = False
      , elementTypes = fields
    }

instance Emitable Type LlvmType where
  emit (TMethod typ) = emit typ
  emit (TBasic typ) = emit typ

instance Emitable TypeMethod LlvmType where
  emit _ = undefined

instance Emitable TypeBasic LlvmType where
  emit (TComposed typ) = emit typ
  emit (TPrimitive typ) = emit typ

instance Emitable TypePrimitive LlvmType where
  emit x = return $ case x of
    TVoid -> VoidType
    TInt -> intType
    TChar -> charType
    TBool -> boolType

instance Emitable TypeComposed LlvmType where
  emit x = case x of
    TObject -> namedType "class.Object"
    TUser (ClassName str) -> namedType str
    TArray typ -> do
      ltyp <- emit typ
      return $ PointerType ltyp defAddrSpace
    TString -> return stringType
    TNull -> Err.unreachable TNull
    where
      namedType = return . NamedTypeReference . Name

instance Emitable Field () where
  emit = undefined

instance Emitable Method () where
  emit Method { methodBody = SInherited } = return ()
  emit Method { methodBody = SBuiltin } = return ()
  emit method@(Method {
        methodName = MethodName namestr
      , methodType = TypeMethod tret _ _
      , methodVariables = vars
      , methodOrigin = origin
      }) = do
    let args = if methodInstance method
        then argSelf : methodArgs method
        else methodArgs method
    NamedTypeReference (Name originstr) <- emit origin
    ltret <- emit tret
    largs <- mapM emitArg args
    blocks <- intercept $ do
      let entry = Name "entry"
      body <- nextUnName
      newBlock entry body $ do
        mapM_ allocVar $ args ++ vars
        mapM_ initArg args
      exit <- nextUnName
      newBlock body exit $ do
        -- FIXME
        return ()
      newBlock exit exit $
        if ltret == VoidType
        then Do |- Ret Nothing
        else do
          def <- defaultValue tret
          Do |- Ret (Just $ ConstantOperand def)
    define $ GlobalDefinition $ functionDefaults {
        name = Name $ concat [originstr, ".", namestr]
      , returnType = ltret
      , parameters = (largs, False)
      , basicBlocks = blocks
    }
      where
        argSelf = Variable (TComposed origin) VariableThis (VariableName "self")
        argName num = Name $ ("arg" ++) $ show $ fromEnum num
        varLocName num = Name $ ("loc" ++) $ show $ fromEnum num
        emitArg (Variable typ num _) = do
          ltyp <- emit typ
          return $ Parameter ltyp (argName num) []
        allocVar (Variable typ num _) = do
          ltyp <- emit typ
          varLocName num |= Alloca ltyp Nothing 8
        initArg (Variable _ num _) =
          Do |- Store False (LocalReference $ varLocName num) (LocalReference $ argName num) Nothing 8

instance Emitable Stmt () where
  emit = undefined

instance Emitable RValue TypeBasic where
  emit = undefined


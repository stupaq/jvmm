{-# LANGUAGE ExistentialQuantification #-}
module Jvmm.LlvmEmitter.Output where

import Control.Monad.Error

import LLVM.General.AST as LLVM
import LLVM.General.Context (withContext)
import LLVM.General.Module (File (..), withModuleFromAST, writeBitcodeToFile)

-- LLVM MODULE --
-----------------
data LlvmModule = LlvmModule LLVM.Module
  deriving (Show)

llvmModuleName :: LlvmModule -> String
llvmModuleName (LlvmModule modu) = moduleName modu

writeUnitFile :: String -> LlvmModule -> IO ()
writeUnitFile file (LlvmModule content) =
  withContext $ \ctx -> handleErrorT $
    withModuleFromAST ctx content $ \m -> handleErrorT $
      writeBitcodeToFile (File file) m
  where
    handleErrorT action = do
      res <- runErrorT action
      case res of
        Left err -> ioError $ userError err
        Right val -> return val


{-# LANGUAGE ExistentialQuantification #-}
module Jvmm.LlvmEmitter.Output where

import Control.Monad.Error

import LLVM.General.AST as LLVM.AST
import LLVM.General.Context
import LLVM.General.Module

-- LLVM MODULE --
-----------------
data LlvmModule = LlvmModule LLVM.AST.Module

handleErrorT :: ErrorT String IO a -> IO a
handleErrorT action = do
  res <- runErrorT action
  case res of
    Left err -> ioError $ userError err
    Right val -> return val

writeUnitFile :: String -> LlvmModule -> IO ()
writeUnitFile file (LlvmModule content) =
  withContext $ \ctx -> handleErrorT $
    withModuleFromAST ctx content $ \m -> handleErrorT $
      writeBitcodeToFile file m


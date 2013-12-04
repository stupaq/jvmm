module Main where

import System.IO (stderr, stdout, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs, getProgName)

import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.IO.Class

import Syntax.AbsJvmm (Program)
import Syntax.ParJvmm (myLexer, pProgram)
import Syntax.PrintJvmm (printTree)
import Syntax.ErrM (Err(..))

import Jvmm.Errors
import Jvmm.Trans
import Jvmm.Trans.Output
import Jvmm.Hierarchy
import Jvmm.Hierarchy.Output
import Jvmm.Scope
import Jvmm.Types
import Jvmm.Analyser
import Jvmm.Verifier
import Jvmm.JvmEmitter
import Jvmm.JvmEmitter.Output
--import Semantics.Runtime

-- WORKFLOWS --
---------------
type Workflow a = String -> ErrorInfoT Identity a

-- Performs parsing of input program
parse :: Workflow Program
parse str =
  let ts = myLexer str
  in case pProgram ts of
    Bad err -> throwError $ Dangling err
    Ok tree -> return tree

-- Performs all static checking and semantics analysis
check :: Workflow ClassHierarchy
check str = parse str >>= trans >>= hierarchy >>= scope >>= typing >>= analyse >>= verify

-- Performs all static analysis and emits Jasmin assembly
compileJvm :: Workflow [JasminAsm]
compileJvm str = check str >>= emitJvm

-- Defaault workflow
defaultWorkflow = compileJvm

-- OUTPUT HELPERS --
--------------------
data Verbosity = Debug | Info | Warn | Error deriving (Eq, Ord, Show)

printl :: Verbosity -> String -> (ReaderT Verbosity IO) ()
printl v s = do
  verb <- ask
  if v >= verb then liftIO $ hPutStrLn stderr s
  else return ()

-- MAIN --
----------
processFile :: Workflow a -> FilePath -> (ReaderT Verbosity IO) ()
processFile workflow f = do
  str <- lift $ readFile f
  case runErrorInfoM $ workflow str of
    Left err -> do
      printl Error $ "ERROR\n"
      printl Error $ show err
      lift exitFailure
    Right _ -> do
      printl Warn $ "OK\n"
      lift exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case take 2 args of
    "-pv":f:[] -> runReaderT (processFile parse f) Debug
    "-p":f:[] -> runReaderT (processFile parse f) Info
    "-cv":f:[] -> runReaderT (processFile check f) Debug
    "-c":f:[] -> runReaderT (processFile check f) Info
    "-v":f:[] -> runReaderT (processFile defaultWorkflow f) Debug
    f:[] -> runReaderT (processFile defaultWorkflow f) Info
    _ -> hPutStrLn stderr "ERROR\n\nbad options format"


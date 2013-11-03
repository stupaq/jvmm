module Main where

import System.IO (stderr, hPutStrLn)
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

import Semantics.Errors
import Semantics.Trans
import Semantics.APTree
import Semantics.Hierarchy
import Semantics.Scope
--import Semantics.Types
--import Semantics.Runtime

-- WORKFLOW --
--------------
parse :: String -> ErrorInfoT Identity Program
parse str =
  let ts = myLexer str
  in case pProgram ts of
    Bad err -> throwError err
    Ok tree -> return tree

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
runFile, parseFile :: FilePath -> (ReaderT Verbosity IO) ()

runFile f = do
  str <- lift $ readFile f
  case runErrorInfoM $ parse str >>= trans >>= hierarchy >>= scope of
    Left err -> do
      printl Error $ "ERROR\n"
      printl Error $ err
    Right eunit -> do
      printl Warn $ "OK\n"
      -- FIXME
      printl Info $ show eunit

parseFile f = do
  str <- lift $ readFile f
  case runErrorInfoM $ parse str of
    Left err -> do
      printl Error $ "ERROR\n"
      printl Error $ err
    Right program -> do
      printl Warn $ "OK\n"
      printl Debug $ show program

main :: IO ()
main = do
  args <- getArgs
  case take 2 args of
    "-p":f:[] -> runReaderT (parseFile f) Info
    "-v":f:[] -> runReaderT (runFile f) Debug
    f:_ -> runReaderT (runFile f) Info
    _ -> hPutStrLn stderr "ERROR\n\nbad options format"


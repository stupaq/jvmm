{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where

import System.IO (stderr, stdout, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs, getProgName)
import System.FilePath ((</>), dropFileName, takeBaseName)
import System.Console.GetOpt

import Text.Show.Pretty (ppShow)

import qualified Data.List as List

import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Applicative

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
type InteractionM a = (ReaderT Options IO) a
type Interaction a = a -> InteractionM ()
type TransformationM a = (ErrorInfoT Identity) a
type Transformation a b = a -> TransformationM b

(=>>) :: Transformation a b -> (Interaction b) -> Interaction a
(=>>) proc sink input =
  case runErrorInfoM $ proc input of
    Left err -> do
      printl Error $ "ERROR\n"
      printl Error $ show err
      lift exitFailure
    Right res -> do
      printl Warn $ "OK\n"
      sink res
      lift exitSuccess

(=>|) :: Interaction a -> a -> InteractionM ()
(=>|) = ($)

nullSink :: Interaction a
nullSink = const (return ())

-- Performs parsing of input program
parseT :: Transformation String Program
parseT str =
  let ts = myLexer str
  in case pProgram ts of
    Bad err -> throwError $ Dangling err
    Ok tree -> return tree
parse :: Interaction String
parse = parseT =>> nullSink

-- Performs all static checking and semantics analysis
checkT :: Transformation String ClassHierarchy
checkT str =
  parseT str >>= trans >>= hierarchy >>= scope >>= typing >>= analyse >>= verify
check :: Interaction String
check = checkT =>> nullSink

-- Performs all static analysis and emits Jasmin assembly
compileJvm :: Interaction String
compileJvm = checkT =>> compile
  where
    compile hierarchy = do
      source <- asks source
      let name = takeBaseName source
      let dest = dropFileName source
      let writeOne = \(JasminAsm clazz lines) -> do
          let file = dest </> clazz
          lift $ writeFile file ""
          mapM_ (lift . appendFile file . toJasmin) lines
      emitJvm name =>> (mapM_ writeOne) =>| hierarchy

-- Defaault workflow
defaultInteraction = compileJvm

-- OUTPUT HELPERS --
--------------------
data Verbosity = Debug | Info | Warn | Error
  deriving (Eq, Ord, Show)

printl :: Verbosity -> String -> InteractionM ()
printl v s = do
  verb <- asks verbosity
  if v >= verb then liftIO $ hPutStrLn stderr s
  else return ()

-- GENERAL OPTIONS --
---------------------
data Options = Options {
    verbosity :: Verbosity
  , workflow :: Interaction String
  , source :: String
}
options0 = Options {
    verbosity = Info
  , workflow = defaultInteraction
  , source = undefined
}

optionsDef :: [OptDescr (Options -> Options)]
optionsDef = [
    Option ['v'] [] (NoArg $ \opts -> opts { verbosity = Debug }) "verbose output"
  , Option ['q'] [] (NoArg $ \opts -> opts { verbosity = Warn }) "quiet output"
  , Option ['p'] [] (NoArg $ \opts -> opts { workflow = parse }) "parse code"
  , Option ['c'] [] (NoArg $ \opts -> opts { workflow = check }) "preform static analysis"
  , Option ['j'] [] (NoArg $ \opts -> opts { workflow = compileJvm }) "preform static analysis"
  ]

parseOptions :: IO Options
parseOptions = do
  prog <- getProgName
  let header = "Usage: " ++ prog ++ " [OPTION...] files..."
  argv <- getArgs
  case getOpt Permute optionsDef argv of
    (opts, [source], []) -> return (List.foldl (flip Prelude.id) (options0 { source = source }) opts)
    _ -> ioError (userError (usageInfo header optionsDef))

-- MAIN --
----------
main :: IO ()
main = do
  opts <- parseOptions
  str <- readFile (source opts)
  runReaderT (workflow opts $ str) opts


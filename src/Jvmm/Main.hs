{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where

import System.Environment (getArgs, getProgName)
import System.FilePath ((</>), dropFileName)
import System.Console.GetOpt

import qualified Data.List as List

import Control.Monad.Error
import Control.Monad.Reader

import Syntax.AbsJvmm (Program)
import Syntax.ParJvmm (myLexer, pProgram)
import Syntax.ErrM (Err(..))

import Jvmm.Workflows
import Jvmm.Errors
import Jvmm.Trans
import Jvmm.Trans.Output
import Jvmm.Hierarchy
import Jvmm.Scope
import Jvmm.Types
import Jvmm.Analyser
import Jvmm.Verifier
import Jvmm.Interpreter
import Jvmm.JvmEmitter
import Jvmm.JvmEmitter.Output

-- WORKFLOWS --
---------------
-- Performs parsing of input program
parseT :: Transformation String Program
parseT str =
  let ts = myLexer str
  in case pProgram ts of
    Bad err -> throwError $ Dangling err
    Ok tree -> return tree
parse :: Interaction String
parse = parseT =?> nullSink

-- Performs all static checking and semantics analysis
checkT :: Transformation String ClassHierarchy
checkT str =
  parseT str >>= trans >>= hierarchy >>= scope >>= typing >>= analyse >>= verify
check :: Interaction String
check = checkT =?> nullSink

-- Performs all static analysis and emits Jasmin assembly
compileJvm :: Interaction String
compileJvm = checkT =?> compile
  where
    compile classes = do
      config <- ask
      let source = configurationSource config
      let dest = dropFileName source
      let writeOne (JasminAsm clazz code) = do
          let file = dest </> clazz ++ ".j"
          lift $ writeFile file $ "; source: " ++ source
          mapM_ (lift . appendFile file . toJasmin) code
          lift $ appendFile file $ "; eof\n"
      emitJvm config =>> mapM_ writeOne =>| classes

execute :: Interaction String
execute = checkT =?> interpret

-- Defaault workflow
defaultInteraction :: Interaction String
defaultInteraction = compileJvm

-- GENERAL OPTIONS --
---------------------
optionsDef :: [OptDescr (Configuration -> Configuration)]
optionsDef = [
    Option "v" [] (NoArg $ \opts -> opts { configurationVerbosity = Debug }) "verbose output"
  , Option "q" [] (NoArg $ \opts -> opts { configurationVerbosity = Warn }) "quiet output"
  , Option "p" [] (NoArg $ \opts -> opts { configurationWorkflow = parse }) "parse code"
  , Option "c" [] (NoArg $ \opts -> opts { configurationWorkflow = check }) "preform static analysis"
  , Option "j" [] (NoArg $ \opts -> opts { configurationWorkflow = compileJvm }) "compile to Jasmin"
  , Option "i" [] (NoArg $ \opts -> opts { configurationWorkflow = execute }) "preform execution"
  , Option "g" [] (NoArg $ \opts -> opts { configurationDebug = True }) "include debug comments in code"
  ]

parseOptions :: IO Configuration
parseOptions = do
  prog <- getProgName
  let header = "\nUsage: " ++ prog ++ " [OPTION...] file"
  argv <- getArgs
  case getOpt Permute optionsDef argv of
    (opts, [source], []) ->
      let baseConfig = configuration0 { configurationSource = source
                                      , configurationWorkflow = defaultInteraction }
      in return (List.foldl (flip Prelude.id) baseConfig opts)
    _ -> ioError (userError (usageInfo header optionsDef))

-- MAIN --
----------
main :: IO ()
main = do
  opts <- parseOptions
  str <- readFile (configurationSource opts)
  runReaderT (configurationWorkflow opts str) opts


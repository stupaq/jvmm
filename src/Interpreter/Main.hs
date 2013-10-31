module Main where
import System.IO (stderr, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs, getProgName)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Either
import Syntax.LexJvmm
import Syntax.ParJvmm
import Syntax.SkelJvmm
import Syntax.PrintJvmm
import Syntax.AbsJvmm
import Syntax.ErrM
import Semantics.Trans
import Semantics.Scope
import Semantics.Types
import Semantics.Runtime

data Verbosity = Debug | Info | Warn | Error deriving (Eq, Ord, Show)

printl :: Verbosity -> String -> (ReaderT Verbosity IO) ()
printl v s = do
  verb <- ask
  if v >= verb then
    liftIO $ hPutStrLn stderr s
  else
    return ()

-- Interpret given file
runFile :: FilePath -> (ReaderT Verbosity IO) ()
runFile f = do
  str <- lift $ readFile f
  run str

run :: String -> (ReaderT Verbosity IO) ()
run s =
  let ts = myLexer s
  in case  pProg ts of
    Bad s -> do
      printl Error $ "ERROR\n"
      printl Error $ s
      printl Error $ "\n[Tokens]\n\n" ++ (show ts)
      liftIO $ exitFailure
    Ok tree ->  case scope $ trans tree of
      Left err -> do
        printl Error $ err
        liftIO $ exitFailure
      Right tree' -> case staticTypes tree' of
        Left err -> do
          printl Error $ err
          liftIO $ exitFailure
        Right tree'' -> do
          printl Warn $ "OK\n"
          verb <- ask
          lift $ runUnit (Info > verb) tree''
          printl Debug $ "\n[Linearized tree]\n\n" ++ show tree'
          printl Debug $ "\n[Type check]\n\n" ++ show tree''

main :: IO ()
main = do
  args <- getArgs
  case take 2 args of
    "-v":f:[] -> runReaderT (runFile f) Debug
    f:_ -> runReaderT (runFile f) Info
    _ -> hPutStrLn stderr "ERROR\n\nbed options format"


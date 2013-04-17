module Main where
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
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

type ParseFun a = [Token] -> Err a

-- Output utils
type Verbosity = Int
printlv :: Verbosity -> String -> IO ()
printlv v s = if v > 1 then putStrLn s else return ()

printl :: String -> IO ()
printl = putStrLn

-- Interpret given file
runFile :: Verbosity -> ParseFun P_Prog -> FilePath -> IO ()
runFile v p f = printl f >> readFile f >>= run v p

run :: Verbosity -> ParseFun P_Prog -> String -> IO ()
run v p s = let ts = myLexer s in case p ts of
  Bad s -> do
    printlv (v - 1) $ "[Tokens]" ++ (show ts)
    printl $ "\n[Error]\n\n" ++ s
  Ok tree ->  case scope $ transAbs tree of
    Right tree' -> do
      printlv (v - 1) $ "\n[Abstract Syntax]\n\n" ++ show tree'
      printlv (v + 1) $ "\n[Linearized tree]\n\n" ++ printTree tree'
      printl $ "\n[Type check]\n\n" ++ (show $ checkTypes tree')
    Left err -> fail err

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= run 2 pP_Prog
    "-q":fs -> runFiles 0 fs
    fs -> runFiles 1 fs
    "-v":fs -> runFiles 2 fs
  where
    runFiles v = mapM_ (runFile v pP_Prog)


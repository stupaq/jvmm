module Main where
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import Syntax.LexJvmm
import Syntax.ParJvmm
import Syntax.SkelJvmm
import Syntax.PrintJvmm
import Syntax.AbsJvmm
import Syntax.ErrM
import Semantics.Trans

type ParseFun a = [Token] -> Err a

-- Output utils
type Verbosity = Int
printlv :: Verbosity -> String -> IO ()
printlv v s = if v > 1 then putStrLn s else return ()

printl :: String -> IO ()
printl = putStrLn

-- Interpret given file
runFile :: Verbosity -> ParseFun PProg -> FilePath -> IO ()
runFile v p f = printl f >> readFile f >>= run v p

run :: Verbosity -> ParseFun PProg -> String -> IO ()
run v p s = let ts = myLexer s in case p ts of
  Bad s -> do
    printl "\nParse Failed...\n"
    printlv (v + 1) "Tokens:"
    printlv v $ show ts
    printl s
  Ok tree -> do
    let tree' = transAbs tree
    printl "\nParse Successful!"
    printlv v $ "\n[Abstract Syntax]\n\n" ++ show tree'
    printlv (v + 1) $ "\n[Linearized tree]\n\n" ++ printTree tree'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= run 2 pPProg
    "-q":fs -> runFiles 0 fs
    fs -> runFiles 1 fs
    "-v":fs -> runFiles 2 fs
  where
    runFiles v = mapM_ (runFile v pPProg)


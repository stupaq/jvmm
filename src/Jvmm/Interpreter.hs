module Jvmm.Interpreter where
import Jvmm.Interpreter.Internal as Internal

import System.Exit

import Control.Monad.State

import qualified Data.Map as Map

import Jvmm.Workflows
import qualified Jvmm.Errors as Err
import Jvmm.Builtins (entrypointName)
import Jvmm.Trans.Output

interpret :: ClassHierarchy -> InteractionM ()
interpret hierarchy = let runenv = buildRunEnv hierarchy in do
  (x, runstate1) <- lift $ runInterpreterM runenv $
      -- Invoke main method
      invokestatic TObject entrypointName []
  let execFailure = liftIO . ioError . userError . show
  let execEnd res = do
      -- Output heap status
      printl Debug "Debug info:"
      printl Debug $ "| Main returned:\t" ++ show res
      printl Debug $ "| Heap stats:\n|\tallocated_count:\t"
          ++ show (Map.size (runenvHeap runstate1 Map.\\ runenvHeap runstate0))
      -- Main result will become the return code of interpreter process
      liftIO $ exitWith $ if res == 0 then ExitSuccess else ExitFailure res
  case x of
    Left (RError err) ->
      if err == Err.userIssuedError
      then liftIO (print err) >> execEnd 1
      else execFailure err
    Left (RException err) -> execFailure $ Err.uncaughtTopLevel err
    Left (RValue (VInt res)) -> execEnd res
    _ -> Err.unreachable x


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
interpret hierarchy = let runenv0 = buildRunEnv hierarchy in do
  (x, runstate1) <- lift $ runInterpreterM runenv0 $ do
      -- Invoke main method
      invokestatic TObject entrypointName []
  case x of
    Left (RError err) -> execFailure err
    Left (RException err) -> execFailure $ Err.uncaughtTopLevel err
    Left (RValue (VInt res)) -> do
      -- Output heap status
      printl Debug $ "+----------------------------------"
      printl Debug $ "| Main returned:\t" ++ show res
      printl Debug $ "| Heap stats:\n|\tallocated_count:\t"
          ++ (show $ Map.size (runenvHeap runstate1 Map.\\ runenvHeap runstate0))
      printl Debug $ "+----------------------------------"
      -- Main result will become the return code of interpreter process
      liftIO $ exitWith $ if res == 0 then ExitSuccess else ExitFailure res
    _ -> Err.unreachable x
  where
    execFailure = liftIO . ioError . userError . show


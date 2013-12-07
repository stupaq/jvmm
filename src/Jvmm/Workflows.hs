module Jvmm.Workflows where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader

import Jvmm.Errors

-- GLOBAL CONFIGURATION --
--------------------------
data Configuration = Configuration {
    configurationVerbosity :: Verbosity
  , configurationWorkflow :: Interaction String
  , configurationSource :: String
  , configurationDebug :: Bool
}

configuration0 :: Configuration
configuration0 = Configuration {
    configurationVerbosity = Info
  , configurationWorkflow = undefined
  , configurationSource = undefined
  , configurationDebug = False
}

-- Terminal output verbosity
data Verbosity = Debug | Info | Warn | Error
  deriving (Eq, Ord, Show)

-- OUTPUT HELPERS --
--------------------
printl :: Verbosity -> String -> InteractionM ()
printl v s = do
  verb <- asks configurationVerbosity
  when (v >= verb) $ liftIO $ hPutStrLn stderr s

-- INTERACTIONS AND TRANSFORMATIONS --
--------------------------------------
type InteractionM a = (ReaderT Configuration IO) a
type Interaction a = a -> InteractionM ()
type TransformationM a = (ErrorInfoT Identity) a
type Transformation a b = a -> TransformationM b

(=?>) :: Transformation a b -> Interaction b -> Interaction a
(=?>) = showFeedback Error Warn

(=>>) :: Transformation a b -> Interaction b -> Interaction a
(=>>) = showFeedback Debug Debug

(=>|) :: Interaction a -> a -> InteractionM ()
(=>|) = ($)

showFeedback :: Verbosity -> Verbosity -> Transformation a b -> Interaction b -> Interaction a
showFeedback verbErr verbOk proc sink input =
  case runErrorInfoM $ proc input of
    Left err -> do
      printl verbErr "ERROR\n"
      printl verbErr (show err)
      lift exitFailure
    Right res -> do
      printl verbOk "OK\n"
      sink res
      lift exitSuccess

-- Discards any input silently
nullSink :: Interaction a
nullSink = const (return ())

module Semantics.Commons where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import System.IO (stderr, hPutStrLn)

-- GENERAL --
-------------
applyAndCompose :: (b -> a -> a) -> [b] -> a -> a
applyAndCompose f = foldl (flip (.)) Prelude.id . map f


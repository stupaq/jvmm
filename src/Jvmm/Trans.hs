module Jvmm.Trans where
import Jvmm.Trans.Internal
import qualified Jvmm.Trans.Output as O

import Control.Monad.Identity

import Syntax.AbsJvmm (Program)

import Jvmm.Errors (ErrorInfoT)

-- Translates AST into APT performing several simplifications and syntactic sugar removal.
trans :: Program -> ErrorInfoT Identity O.CompilationUnit
trans program = return $ tProgram program


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Jvmm.Verifier.Internal where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Traversable as Traversable

import Jvmm.Builtins (isEntrypoint)
import qualified Jvmm.Errors as Err
import Jvmm.Errors (rethrow, ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output
import qualified Jvmm.Types.Internal as Types

-- This layer performs static checks that do not alter Abstract Program Tree.

-- STATIC INFORMATION REPRESENTATION --
---------------------------------------
data VerifierState = VerifierState {
    -- This keeps trach whether functions actually returned
    verifierstateReturned :: Bool
    -- Determine whether entrypoint is present
  , verifierstateMain :: Bool
}

verifierstate0 = VerifierState {
    verifierstateReturned = False
  , verifierstateMain = False
}

-- ANALYSER MONAD --
--------------------
type VerifierM = StateT VerifierState (ErrorInfoT Identity)
runVerifierM :: VerifierM a -> ErrorInfoT Identity a
runVerifierM m = fmap fst $ runStateT m verifierstate0

probeReturned :: VerifierM a -> VerifierM (a, Bool)
probeReturned action = do
  curr <- gets verifierstateReturned
  setReturned False
  res <- action
  returned <- gets verifierstateReturned
  setReturned curr
  return (res, returned)

checkReturned :: TypeBasic -> VerifierM a -> VerifierM a
checkReturned (TPrimitive TVoid) action = action
checkReturned _ action = do
  (res, returned) <- probeReturned action
  guard returned `rethrow` Err.missingReturn
  return res

clearReturned :: VerifierM a -> VerifierM a
clearReturned = fmap fst . probeReturned

setReturned, orReturned :: Bool -> VerifierM ()
setReturned b = modify (\st -> st { verifierstateReturned = b })
orReturned b = gets verifierstateReturned >>= (setReturned . (|| b))

checkEntrypoint :: Method -> VerifierM ()
checkEntrypoint method
  | isEntrypoint method = modify (\st -> st { verifierstateMain = True })
checkEntrypoint _ = return ()

-- TRAVERSING TREE --
---------------------
class Verifiable a where
  verify :: a -> VerifierM ()

instance Verifiable ClassHierarchy where
  verify hierarchy = do
    Traversable.mapM verify hierarchy
    (gets verifierstateMain >>= guard) `rethrow` Err.missingMain

instance Verifiable Class where
  verify Class { classAllMethods = methods, classLocation = loc } =
    Err.withLocation loc (mapM_ verify methods)

instance Verifiable Method where
  verify method@Method { methodBody = stmt, methodType = typ , methodLocation = loc } =
    Err.withLocation loc $ do
      checkEntrypoint method
      let TypeMethod rett _ _ = typ
      checkReturned rett $ verify stmt

instance Verifiable Stmt where
  verify x = case x of
    SBlock stmts -> mapM_ verify stmts
    -- Memory access
    -- Control statements
    SReturn _ _ -> setReturned True
    SReturnV -> setReturned True
    SIf ELitTrue stmt -> verify stmt -- TODO analyser should resolve this
    SIf _ stmt ->
      -- Whether this statement was executed depends on runtime evaluation of expression
      clearReturned $ verify stmt
    SIfElse ELitTrue stmt1 _ -> verify stmt1 -- TODO analyser should resolve this
    SIfElse ELitFalse _ stmt2 -> verify stmt2 -- TODO analyser should resolve this
    SIfElse _ stmt1 stmt2 -> do
      (_, retd1) <- probeReturned $ verify stmt1
      (_, retd2) <- probeReturned $ verify stmt2
      orReturned (retd1 && retd2)
    -- We have no break nor goto instruction therefore infinite loop will either loop or return or
    -- terminate entire program with 'runtime error' (which I consider as returning)
    SWhile ELitTrue stmt -> do
      verify stmt
      orReturned True
    SWhile _ stmt ->
      -- Whether this statement was executed depends on runtime evaluation of expression
      clearReturned $ verify stmt
    -- We consider throw statement to be equivalent to return for the sake of checking whether the
    -- function returns (we do allow function to throw and not return in the dead code after throw)
    SThrow _ -> orReturned True
    -- Since we cannot statically exclude possibility of exception being thrown, we require try {}
    -- block to return or throw AND catch () {} block to return
    STryCatch stmt1 _ _ stmt2 -> do
      (_, retd1) <- probeReturned $ verify stmt1
      (_, retd2) <- probeReturned $ verify stmt2
      orReturned (retd1 && retd2)
    -- Special function bodies
    SBuiltin -> setReturned True
    SInherited -> setReturned True
    -- Metainformation carriers
    SMetaLocation loc stmts -> Err.withLocation loc (mapM_ verify stmts)
    -- These statements will be replaced with ones caring more context in subsequent phases
    T_SDeclVar _ _ -> Err.unreachable x
    -- Nothing to do here
    _ -> return ()


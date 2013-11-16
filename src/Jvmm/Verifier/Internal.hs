module Jvmm.Verifier.Internal where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Traversable as Traversable

import Jvmm.Builtins (entrypointIdent)
import qualified Jvmm.Errors as Err
import Jvmm.Errors (rethrow, ErrorInfoT, addLocation)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

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

checkReturned :: Type -> VerifierM a -> VerifierM a
checkReturned TVoid action = action
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
checkEntrypoint Method { methodIdent = ident, methodOrigin = TUnknown }
  | ident  == entrypointIdent = modify (\st -> st { verifierstateMain = True })
checkEntrypoint _ = return ()

-- TRAVERSING TREE --
---------------------
funH :: ClassHierarchy -> VerifierM ()
funH classes = do
  Traversable.mapM
    (\Class { classMethods = methods, classStaticMethods = staticMethods } -> do
        mapM_ funM methods >> mapM_ funSM staticMethods)
    classes
  (gets verifierstateMain >>= guard) `rethrow` Err.missingMain

funM, funSM :: Method -> VerifierM ()
funM method@Method { methodBody = stmt, methodType = typ } = do
  let TFunc rett _ _ = typ
  checkReturned rett $ funS stmt
funSM method = do
  checkEntrypoint method
  funM method

funS :: Stmt -> VerifierM ()
funS x = case x of
  SLocal _ stmts -> mapM_ funS stmts
  SReturn _ -> setReturned True
  SIf ELitTrue stmt -> funS stmt -- TODO analyser resolves this
  SIf _ stmt ->
    -- Whether this statement was executed depends on runtime evaluation of expression
    clearReturned $ funS stmt
  SIfElse ELitTrue stmt1 stmt2 -> funS stmt1 -- TODO analyser resolves this
  SIfElse ELitFalse stmt1 stmt2 -> funS stmt2 -- TODO analyser resolves this
  SIfElse _ stmt1 stmt2 -> do
    (_, retd1) <- probeReturned $ funS stmt1
    (_, retd2) <- probeReturned $ funS stmt2
    orReturned (retd1 && retd2)
  SWhile ELitTrue stmt -> funS stmt
  SWhile _ stmt ->
    -- Whether this statement was executed depends on runtime evaluation of expression
    clearReturned $ funS stmt
  -- We consider throw statement to be equivalent to return for the sake of checking whether the
  -- function returns (we do allow function to throw and not return in the dead code after throw)
  SThrow _ -> orReturned True
  -- Since we cannot statically exclude possibility of exception being thrown, we require try {}
  -- block to return or throw AND catch () {} block to return
  STryCatch stmt1 _ _ stmt2 -> do
    (_, retd1) <- probeReturned $ funS stmt1
    (_, retd2) <- probeReturned $ funS stmt2
    orReturned (retd1 && retd2)
  SReturnV -> setReturned True
  SBuiltin -> setReturned True
  SInherited -> setReturned True
  SMetaLocation loc stmts -> (mapM_ funS stmts) `addLocation` loc
  SDeclVar _ _ -> error $ Err.unusedBranch x
  _ -> return ()

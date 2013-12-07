{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Jvmm.Analyser.Internal where

import Control.Monad.Identity
import Control.Monad.State
import Control.Applicative

import qualified Data.Traversable as Traversable

import qualified Jvmm.Errors as Err
import Jvmm.Errors (rethrow, ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- Implements static program transformations, for now it includes:
-- - partial evaluation of constant expressions
-- - prunning of unreachable branches and no-op loops

-- THE STATE --
--------------------
data AnalyserState = AnalyserState {
    analyserstateReachable :: Bool
} deriving (Show)

analyserstate0 = AnalyserState True :: AnalyserState

-- THE MONAD --
--------------------
type AnalyserM = StateT AnalyserState (ErrorInfoT Identity)
runAnalyserM :: AnalyserM a -> ErrorInfoT Identity a
runAnalyserM m = fmap fst $ runStateT m analyserstate0

setReachable :: Bool -> AnalyserM ()
setReachable b = modify (\st -> st { analyserstateReachable = b })
isReachable :: AnalyserM Bool
isReachable = gets analyserstateReachable

-- TREE REWRITING --
--------------------
class Analysable a b | a -> b where
  analyse :: a -> AnalyserM b

instance Analysable ClassHierarchy ClassHierarchy where
  analyse = Traversable.mapM analyse

instance Analysable Class Class where
  analyse clazz@Class { classAllMethods = methods, classLocation = loc } =
    Err.withLocation loc $ do
      methods' <- mapM analyse methods
      return clazz { classAllMethods = methods' }

instance Analysable Method Method where
  analyse method@Method { methodBody = stmt, methodLocation = loc } =
    Err.withLocation loc $ do
      stmt' <- wrapStmts <$> analyse stmt
      return method { methodBody = stmt' }
        where
          wrapStmts [stmt] = stmt
          wrapStmts stmts = SMetaLocation loc stmts

instance Analysable Stmt [Stmt] where
  analyse x = case x of
    SEmpty -> nothing
    SBlock stmts -> single $ SBlock <$> sequence stmts
    SExpr expr -> single $ SExpr <$> analyse expr
    -- Memory access
    SStore num expr typ -> single $ SStore num <$> analyse expr <#> typ
    SStoreArray num expr1 expr2 telem -> single $
      SStoreArray num <$> analyse expr1 <*> analyse expr2 <#> telem
    SPutField num ctyp name expr ftyp -> single $
      SPutField num ctyp name <$> analyse expr <#> ftyp
    -- Control statements
    SReturn expr typ -> do
      expr' <- analyse expr
      setReachable False
      return [SReturn expr' typ]
    SReturnV -> do
      setReachable False
      original
    SIf expr stmt -> do
      expr' <- analyse expr
      case expr' of
        ELitTrue -> analyse stmt
        ELitFalse -> nothing
        _ -> do
          -- Expr might have some side effects and is not statically known which branch will be taken
          stmts' <- analyse stmt
          case stmts' of
            -- Thie expression cannot just dissapear
            [] -> return [SExpr expr']
            _ -> return [SIf expr' $ block stmts']
    SIfElse expr stmt1 stmt2 -> do
      expr' <- analyse expr
      case expr' of
        ELitTrue -> analyse stmt1
        ELitFalse -> analyse stmt2
        _ -> do
          stmtsPair <- liftM2 (,) (analyse stmt1) (analyse stmt2)
          case stmtsPair of
            ([], []) -> return [SExpr expr']
            (stmts1', []) -> return [SIf expr' (block stmts1')]
            ([], stmts2') -> return [SIf (EUnary OuNot expr' (TPrimitive TBool)) (block stmts2')]
            (stmts1', stmts2') -> return [SIfElse expr' (block stmts1') (block stmts2')]
    SWhile expr stmt -> do
      expr' <- analyse expr
      case expr' of
        ELitFalse -> nothing
        _ -> do
          stmt' <- block <$> analyse stmt
          -- No prunning of loop's body can be done here
          -- However if it loops forever, the instructions after the loop are not reachable
          when (isLitTrue expr') $ setReachable False
          return [SWhile expr' stmt']
    SThrow {} -> original
    STryCatch {} -> original
    -- Special function bodies
    SBuiltin -> original
    SInherited -> original
    -- Metainformation carriers
    SMetaLocation loc stmts -> Err.withLocation loc (sequence stmts)
    -- These statements will be replaced with ones caring more context in subsequent phases
    T_SDeclVar {} -> Err.unreachable x
    T_SAssign {} -> Err.unreachable x
    T_SAssignArr {} -> Err.unreachable x
    T_SAssignFld {} -> Err.unreachable x
    T_STryCatch {} -> Err.unreachable x
    where
      original, nothing :: AnalyserM [Stmt]
      original = return [x]
      nothing = return []
      single :: AnalyserM Stmt -> AnalyserM [Stmt]
      single = fmap (:[])
      sequence :: [Stmt] -> AnalyserM [Stmt]
      sequence = liftM concat . mapM analyse
      block :: [Stmt] -> Stmt
      block [] = SEmpty
      block [stmt] = stmt
      block stmts = SBlock stmts
      isLitTrue :: Expr -> Bool
      isLitTrue ELitTrue = True
      isLitTrue _ = False

instance Analysable Expr Expr where
  analyse x = case x of
    -- Literals
    -- Memory access
    EArrayLoad expr1 expr2 telem -> EArrayLoad <$> analyse expr1 <*> analyse expr2 <#> telem
    EGetField expr ctyp name ftyp -> EGetField <$> analyse expr <#> ctyp <#> name <#> ftyp
    -- Method calls
    EInvokeStatic ctyp name ftyp exprs -> EInvokeStatic ctyp name ftyp <$> mapM analyse exprs
    EInvokeVirtual expr ctyp name ftyp exprs ->
      EInvokeVirtual <$> analyse expr <#> ctyp <#> name <#> ftyp <*> mapM analyse exprs
    -- Object creation
    ENewArr telem expr -> ENewArr telem <$> analyse expr
    -- Operations
    EUnary op expr tret -> do
      expr' <- analyse expr
      case (expr', op) of
        (ELitInt n, OuNeg) -> return $ ELitInt (0 - n)
        (ELitTrue, OuNot) -> return ELitFalse
        (ELitFalse, OuNot) -> return ELitTrue
        _ -> return $ EUnary op expr' tret
    EBinary op expr1 expr2 tret -> do
      option <- liftM3 (,,) (analyse expr1) (analyse expr2) (return op)
      case option of
        (expr1', expr2', op) -> return $ EBinary op expr1' expr2' tret
    -- These expressions will be replaced with ones caring more context in subsequent phases
    T_EVar {} -> Err.unreachable x
    -- Fallback to original value
    _ -> original
    where
      original :: AnalyserM Expr
      original = return x

-- PORN --
----------
infixl 4 <#>
(<#>) x y = x <*> pure y


{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Jvmm.Analyser.Internal where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State

import qualified Data.Traversable as Traversable

import Jvmm.Errors (ErrorInfoT)
import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output

-- THE STATE --
---------------
data AnalyserState = AnalyserState {
    analyserstateReachable :: Bool
} deriving (Show)

analyserstate0 :: AnalyserState
analyserstate0 = AnalyserState True

-- THE MONAD --
---------------
type AnalyserM = StateT AnalyserState (ErrorInfoT Identity)
runAnalyserM :: AnalyserM a -> ErrorInfoT Identity a
runAnalyserM m = fmap fst $ runStateT m analyserstate0

setReachable :: Bool -> AnalyserM ()
setReachable b = modify (\st -> st { analyserstateReachable = b })
isReachable :: AnalyserM Bool
isReachable = gets analyserstateReachable
-- TODO make use of this information in consecutive instructions

-- TREE REWRITING --
--------------------
class Analysable a b | a -> b where
  analyse :: a -> AnalyserM b

instance Analysable ClassHierarchy ClassHierarchy where
  analyse = Traversable.mapM analyse

instance Analysable Class Class where
  analyse clazz@Class { classAllMethods = methods, classLocation = loc } =
    Err.withLocation loc $ do
      setReachable True
      methods' <- mapM analyse methods
      return clazz { classAllMethods = methods' }

instance Analysable Method Method where
  analyse method@Method { methodBody = stmt, methodLocation = loc } =
    Err.withLocation loc $ do
      -- Process method body
      stmts' <- analyse stmt
      return method { methodBody = wrapStmts stmts' }
        where
          wrapStmts [x] = x
          wrapStmts x = SMetaLocation loc x

instance Analysable Stmt [Stmt] where
  analyse x = case x of
    SEmpty -> nothing
    SBlock stmts ->
     -- Blocks have no semantic value after resolving scope
     consecutive stmts
    SExpr expr typ -> single $ SExpr <$> analyse expr <#> typ
    -- Memory access
    SAssign lval expr typ -> single $ SAssign <$> analyse lval <*> analyse expr <#> typ
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
          stmts' <- analyse stmt
          case stmts' of
            -- This expression cannot just dissapear
            [] -> return [SExpr expr' (TPrimitive TBool)]
            _ -> return [SIf expr' $ block stmts']
    SIfElse expr stmt1 stmt2 -> do
      expr' <- analyse expr
      case expr' of
        ELitTrue -> analyse stmt1
        ELitFalse -> analyse stmt2
        _ -> do
          stmtsPair <- liftM2 (,) (analyse stmt1) (analyse stmt2)
          case stmtsPair of
            ([], []) -> return [SExpr expr' (TPrimitive TBool)]
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
    SMetaLocation loc stmts -> Err.withLocation loc (consecutive stmts)
    -- These statements will be replaced with ones caring more context in subsequent phases
    PruneSDeclVar {} -> Err.unreachable x
    PruneSTryCatch {} -> Err.unreachable x
    where
      original, nothing :: AnalyserM [Stmt]
      original = return [x]
      nothing = return []
      single :: AnalyserM Stmt -> AnalyserM [Stmt]
      single = fmap (:[])
      consecutive :: [Stmt] -> AnalyserM [Stmt]
      consecutive = liftM concat . mapM analyse
      block :: [Stmt] -> Stmt
      block [] = SEmpty
      block [stmt] = stmt
      block stmts = SBlock stmts
      isLitTrue :: RValue -> Bool
      isLitTrue ELitTrue = True
      isLitTrue _ = False

instance Analysable RValue RValue where
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
        (ELitInt n, OuNeg) -> return $ ELitInt (negate n)
        (ELitTrue, OuNot) -> return ELitFalse
        (ELitFalse, OuNot) -> return ELitTrue
        _ -> return $ EUnary op expr' tret
    EBinary op expr1 expr2 tret -> do
      option <- liftM3 (,,) (return op) (analyse expr1) (analyse expr2)
      -- Note that comparint subtrees is meaningless here (this is one of the reasons for RValue
      -- not being instance of Eq) as the language is not purely functional
      case option of
        (ObAnd, ELitFalse, _) -> return ELitFalse
        (ObAnd, ELitTrue, expr2') -> return expr2'
        (ObOr, ELitTrue, _) -> return ELitTrue
        (ObOr, ELitFalse, expr2') -> return expr2'
        -- TODO constant propagation
        (_, expr1', expr2') -> return $ EBinary op expr1' expr2' tret
    -- These expressions will be replaced with ones caring more context in subsequent phases
    PruneEVar {} -> Err.unreachable x
    -- Fallback to original value
    _ -> original
    where
      original :: AnalyserM RValue
      original = return x

instance Analysable LValue LValue where
  analyse x = case x of
    LVariable _ _ -> return x
    LArrayElement lval expr telem -> LArrayElement <$> analyse lval <*> analyse expr <#> telem
    LField lval ctyp name ftyp -> LField <$> analyse lval <#> ctyp <#> name <#> ftyp
    PruneLExpr _ -> Err.unreachable x


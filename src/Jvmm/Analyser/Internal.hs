{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Jvmm.Analyser.Internal where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State

import qualified Data.List as List
import qualified Data.Map as Map

import qualified Data.Traversable as Traversable

import qualified Jvmm.Builtins as Builtins
import Jvmm.Errors (ErrorInfoT)
import qualified Jvmm.Errors as Err
import Jvmm.Trans.Output

-- THE STATE --
---------------
data AnalyserState = AnalyserState {
    analyserstateReachable :: Bool
  , analyserstateValues    :: ConstantValues
} deriving (Show)

type ConstantValues = Map.Map VariableNum RValue

analyserstate0 :: AnalyserState
analyserstate0 = AnalyserState True Map.empty

-- THE MONAD --
---------------
type AnalyserM = StateT AnalyserState (ErrorInfoT Identity)
runAnalyserM :: AnalyserM a -> ErrorInfoT Identity a
runAnalyserM m = fst <$> runStateT m analyserstate0

localValues :: AnalyserM a -> AnalyserM (a, ConstantValues)
localValues action = do
  v <- gets analyserstateValues
  r <- action
  v' <- gets analyserstateValues
  modify $ \s -> s { analyserstateValues = v }
  return (r, v')

setReachable :: Bool -> AnalyserM ()
setReachable b = modify (\st -> st { analyserstateReachable = b })
isReachable :: AnalyserM Bool
isReachable = gets analyserstateReachable
-- TODO make use of this information in consecutive instructions

setValue :: VariableNum -> RValue -> AnalyserM ()
setValue num rval =
  modify $ \s -> s { analyserstateValues = Map.insert num rval $ analyserstateValues s }

resetValue :: VariableNum -> AnalyserM ()
resetValue num = modify $ \s -> s {
  analyserstateValues = Map.delete num $ analyserstateValues s }

mergeValues :: ConstantValues -> AnalyserM ()
mergeValues vals2 = do
  vals1 <- gets analyserstateValues
  let left = Map.fromList $ List.intersect (Map.toList vals1) (Map.toList vals2)
  modify $ \s -> s { analyserstateValues = left }

getValue :: VariableNum -> RValue -> AnalyserM RValue
getValue num rval = do
  vals <- gets analyserstateValues
  return $ Map.findWithDefault rval num vals

getCopies :: VariableNum -> AnalyserM [VariableNum]
getCopies num = do
  vals <-  gets analyserstateValues
  return $ map fst $ filter filterCopies $ Map.toList vals
    where
      filterCopies (_, ELoad num' _) = num == num'
      filterCopies _ = False

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
  analyse method@Method { methodBody = stmt, methodLocation = loc, methodVariables = vars } =
    Err.withLocation loc $ do
      -- Process method body
      (stmts', _) <- localValues $ do
        forM_ vars $ \(Variable typ num _) -> setValue num $ Builtins.defaultValue typ
        analyse stmt
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
    SExpr _e typ -> single $ SExpr <$> analyse _e <#> typ
    -- Memory access
    SAssign lval@(LVariable num typ) _e _ -> do
      _e <- analyse _e
      -- Update constant and copy propagation map
      mapM_ resetValue =<< getCopies num
      if isLiteral _e || isCopy _e
      then setValue num _e
      else resetValue num
      single $ SAssign <$> analyse lval <#> _e <#> typ
      where
        isCopy (ELoad _ _) = True
        isCopy _ = False
    SAssign lval _e typ -> single $ SAssign <$> analyse lval <*> analyse _e <#> typ
    -- Control statements
    SReturn _e typ -> do
      _e <- analyse _e
      setReachable False
      return [SReturn _e typ]
    SReturnV -> do
      setReachable False
      original
    SIf _e stmt -> do
      _e <- analyse _e
      case _e of
        ELitTrue -> analyse stmt
        ELitFalse -> nothing
        _ -> do
          (stmts', vals') <- localValues $ analyse stmt
          mergeValues vals'
          case stmts' of
            -- This expression cannot just dissapear
            [] -> return [SExpr _e (TPrimitive TBool)]
            _ -> return [SIf _e $ block stmts']
    SIfElse _e stmt1 stmt2 -> do
      _e <- analyse _e
      case _e of
        ELitTrue -> analyse stmt1
        ELitFalse -> analyse stmt2
        _ -> do
          (stmt1', vals1') <- localValues $ analyse stmt1
          (stmt2', vals2') <- localValues $ analyse stmt2
          mergeValues vals1'
          mergeValues vals2'
          case (stmt1', stmt2') of
            ([], []) -> return [SExpr _e (TPrimitive TBool)]
            (_, []) -> return [SIf _e (block stmt1')]
            ([], _) -> return [SIf (EUnary OuNot _e (TPrimitive TBool)) (block stmt2')]
            (_, _') -> return [SIfElse _e (block stmt1') (block stmt2')]
    SWhile expr stmt -> do
      _e <- analyse expr
      case _e of
        ELitFalse -> nothing
        _ -> do
          mapM_ resetValue $ killedVars stmt
          (stmt', _) <- localValues $ analyse stmt
          -- We have to reevaluate loop guard
          _e <- analyse expr
          -- No prunning of loop's body can be done here
          -- However if it loops forever, the instructions after the loop are not reachable
          when (isLitTrue _e) $ setReachable False
          return [SWhile _e $ block stmt']
    SThrow {} -> original
    STryCatch stmt1 _ _ stmt2 -> do
      mapM_ resetValue $ killedVars stmt1 ++ killedVars stmt2
      original
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

killedVars :: Stmt -> [VariableNum]
killedVars x = case x of
  SEmpty -> []
  SBlock stmts -> concatMap killedVars stmts
  SExpr _ _ -> []
  -- Memory access
  SAssign (LVariable num _) _ _  -> [num]
  SAssign {} -> []
  -- Control statements
  SReturn _ _ -> []
  SReturnV -> []
  SIf _ stmt -> killedVars stmt
  SIfElse _ stmt1 stmt2 -> concatMap killedVars [stmt1, stmt2]
  SWhile _ stmt -> killedVars stmt
  SThrow _ -> []
  STryCatch stmt1 _ _ stmt2 -> concatMap killedVars [stmt1, stmt2]
  -- Special function bodies
  SBuiltin -> []
  SInherited -> []
  -- Metainformation carriers
  SMetaLocation _ stmts -> concatMap killedVars stmts
  -- These statements will be replaced with ones caring more context in subsequent phases
  PruneSDeclVar {} -> Err.unreachable x
  PruneSTryCatch {} -> Err.unreachable x

class Constant a where
  asConst :: a -> AnalyserM RValue

instance Constant Bool where
  asConst x = return $ if x then ELitTrue else ELitFalse

instance Constant Integer where
  asConst = return . ELitInt

instance Constant String where
  asConst = return . ELitString

instance Analysable RValue RValue where
  analyse x = case x of
    -- Literals
    -- Memory access
    ELoad num _ -> getValue num x
    EArrayLoad _e1 _e2 telem -> EArrayLoad <$> analyse _e1 <*> analyse _e2 <#> telem
    EGetField _e ctyp name ftyp -> EGetField <$> analyse _e <#> ctyp <#> name <#> ftyp
    -- Method calls
    EInvokeStatic ctyp name ftyp _es -> EInvokeStatic ctyp name ftyp <$> mapM analyse _es
    EInvokeVirtual _e ctyp name ftyp _es ->
      EInvokeVirtual <$> analyse _e <#> ctyp <#> name <#> ftyp <*> mapM analyse _es
    -- Object creation
    ENewArr telem _e -> ENewArr telem <$> analyse _e
    -- Operations
    EUnary op _e tret -> do
      _e <- analyse _e
      case (op, _e) of
        -- Bool
        (OuNot, ELitTrue) -> asConst False
        (OuNot, ELitFalse) -> asConst True
        -- Int
        (OuNeg, ELitInt _e) -> asConst $ negate _e
        -- Fallback
        _ -> return $ EUnary op _e tret
    EBinary op _e1 _e2 tret typ1 typ2 -> do
      option@(_, _e1, _e2) <- liftM3 (,,) (return op) (analyse _e1) (analyse _e2)
      let fallback = return $ EBinary op _e1 _e2 tret typ1 typ2
      -- Note that comparing subtrees is meaningless here (this is one of the reasons for RValue
      -- not being instance of Eq) as the language is not purely functional
      case option of
        -- No guarantees whether expressions are constant
        -- Somethimes we can tell for sure that both sides are the same
        (ob, ELoad num1 _, ELoad num2 _)
          | num1 == num2 -> case ob of
            ObEQU -> asConst True
            ObNEQ -> asConst False
            ObLTH -> asConst False
            ObLEQ -> asConst True
            ObGTH -> asConst False
            ObGEQ -> asConst True
            _ -> fallback
          | otherwise -> fallback
        -- Bool
        (ObAnd, ELitFalse, _) -> asConst False
        (ObAnd, ELitTrue, _e2) -> return _e2
        (ObOr, ELitTrue, _) -> asConst True
        (ObOr, ELitFalse, _e2) -> return _e2
        (_, _e1, _e2) -> if isLiteral _e1 && isLiteral _e2
          then case option of
            -- We have guarantee that subexpressions are constants
            -- Object
            (ObEQU, ENull _, ENull _) -> asConst True
            (ObNEQ, ENull _, ENull _) -> asConst False
            -- String
            (ObPlus, ELitString str1, ELitString str2) -> asConst $ str1 ++ str2
            -- Bool
            (ObEQU, ELitTrue, _e2) -> return _e2
            (ObNEQ, ELitFalse, _e2) -> return _e2
            (ob, ELitTrue, ELitTrue) -> asConst $ ob == ObEQU
            (ob, ELitFalse, ELitFalse) -> asConst $ ob == ObEQU
            (ob, ELitTrue, ELitFalse) -> asConst $ ob == ObNEQ
            (ob, ELitFalse, ELitTrue) -> asConst $ ob == ObNEQ
            -- Int
            (ObEQU, ELitInt x1, ELitInt x2) -> asConst $ x1 == x2
            (ObNEQ, ELitInt x1, ELitInt x2) -> asConst $ x1 /= x2
            (ObLTH, ELitInt x1, ELitInt x2) -> asConst $ x1 < x2
            (ObLEQ, ELitInt x1, ELitInt x2) -> asConst $ x1 <= x2
            (ObGTH, ELitInt x1, ELitInt x2) -> asConst $ x1 > x2
            (ObGEQ, ELitInt x1, ELitInt x2) -> asConst $ x1 >= x2
            (ObPlus, ELitInt x1, ELitInt x2) -> asConst $ x1 + x2
            (ObMinus, ELitInt x1, ELitInt x2) -> asConst $ x1 - x2
            (ObTimes, ELitInt x1, ELitInt x2) -> asConst $ x1 * x2
            (ObDiv, ELitInt x1, ELitInt x2) -> asConst $ x1 `div` x2
            (ObMod, ELitInt x1, ELitInt x2) -> asConst $ x1 `rem` x2
            -- Char
            (ObEQU, ELitChar x1, ELitChar x2) -> asConst $ x1 == x2
            (ObNEQ, ELitChar x1, ELitChar x2) -> asConst $ x1 /= x2
            _ -> fallback
          else fallback
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
    LArrayElement lval _e telem -> LArrayElement <$> analyse lval <*> analyse _e <#> telem
    LField lval ctyp name ftyp -> LField <$> analyse lval <#> ctyp <#> name <#> ftyp
    PruneLExpr _ -> Err.unreachable x


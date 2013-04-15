module Semantics.Trans where
import Syntax.AbsJvmm

-- After running translate on AST one can assume non-existance of all labels
-- prefixed with "P" (type separation might be introduced in the future)
transAbs :: PProg -> Stmt
transAbs = transPProg
  where
    transPProg :: PProg -> Stmt
    transPProg (PProg pdeffuncs) = Global $ map transPDefFunc pdeffuncs

    transPDefFunc :: PDefFunc -> Stmt
    transPDefFunc (PDefFunc typ id args pblock) = SDefFunc typ id args $ transPBlock pblock

    transStmt :: Stmt -> [Stmt]
    transStmt x = case x of
      PSDeclVar typ pitems -> map (transPItem typ) pitems
      SBlock stmts -> return $ SBlock $ do
        stmt <- stmts
        transStmt stmt
      PSBlock pblock -> return $ transPBlock pblock
      SIf expr stmt -> return $ SIf expr $ transStmt' stmt
      SIfElse expr stmt1 stmt2 -> return $ SIfElse expr (transStmt' stmt1) (transStmt' stmt2)
      SWhile expr stmt -> return $ SWhile expr $ transStmt' stmt
      SForeach typ id expr stmt -> return $ SForeach typ id expr $ transStmt' stmt
      _ -> return x

    transStmt' :: Stmt -> Stmt
    transStmt' x = case transStmt x of
      [stmt] -> stmt
      stmts -> SBlock stmts

    transPItem :: Type -> PItem -> Stmt
    transPItem typ x = case x of
      PNoInit id -> SDeclVar typ id
      PInit id expr -> SDefVar typ id expr

    transPBlock :: PBlock -> Stmt
    transPBlock (PBlock stmts) = SBlock $ do
      stmt <- stmts
      transStmt stmt


{-# OP_TIONS_GHC -fwarn-incomplete-patterns #-}
module Semantics.Trans where
import Syntax.AbsJvmm

-- Unified identifier
data UIdent =
    VIdent String
  | FIdent String
  | TIdent String
  deriving (Ord, Eq, Show)

toStr :: Ident -> String
toStr (Ident id) = id

-- After running translate on AST one can assume non-existance of all labels
-- prefixed with "P" (type separation might be introduced in the future)
transAbs :: P_Prog -> Stmt
transAbs = transP_Prog
  where
    transP_Prog :: P_Prog -> Stmt
    transP_Prog (P_Prog pdeffuncs) = Global $ map transP_DefFunc pdeffuncs

    transP_DefFunc :: P_DefFunc -> Stmt
    transP_DefFunc (P_DefFunc typ id args (P_Excepts excepts) pblock) = SDefFunc typ id args excepts $ transP_Block pblock
    transP_DefFunc (P_DefFunc typ id args (P_NoExcept) pblock) = SDefFunc typ id args [] $ transP_Block pblock

    transStmt :: Stmt -> [Stmt]
    transStmt x = case x of
      P_SDeclVar typ pitems -> concat $ map (transP_Item typ) pitems
      P_SBlock pblock -> return $ transP_Block pblock
      SIf expr stmt -> return $ SIf expr $ transStmt' stmt
      SIfElse expr stmt1 stmt2 -> return $ SIfElse expr (transStmt' stmt1) (transStmt' stmt2)
      SWhile expr stmt -> return $ SWhile expr $ transStmt' stmt
      SForeach typ id expr stmt -> return $ SForeach typ id expr $ transStmt' stmt
      SThrow expr -> return $ SThrow expr
      STryCatch stmt1 typ2 id3 stmt4 -> return $ STryCatch (transStmt' stmt1) typ2 id3 (transStmt' stmt4)
      Local _ _ -> undefined
      P1_SBlock stmts -> undefined
      _ -> return x

    transStmt' :: Stmt -> Stmt
    transStmt' x = case transStmt x of
      [stmt] -> stmt
      stmts -> P1_SBlock stmts

    transP_Item :: Type -> P_Item -> [Stmt]
    transP_Item typ x = case x of
      P_NoInit id -> [SDeclVar typ id]
      P_Init id expr -> [SDeclVar typ id, SAssign id expr]

    transP_Block :: P_Block -> Stmt
    transP_Block (P_Block stmts) = P1_SBlock $ do
      stmt <- stmts
      transStmt stmt


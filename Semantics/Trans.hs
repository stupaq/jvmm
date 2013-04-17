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

-- Performs abstract syntax tree transformations that do not alter semantics
-- and simplify its structure.  After running translate on AST one can assume
-- non-existance of all labels prefixed with "P_" (type separation might be
-- introduced in the future)
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
      -- internal
      Local stmts1 stmts2  -> undefined
      Global stmts  -> undefined
      SDefFunc type' id args types stmt  -> undefined
      SDeclVar type' id  -> undefined
      -- transform
      P_SDeclVar type' p_items  ->  concat $ map (transP_Item type') p_items
      P_SBlock p_block  ->  return $ transP_Block p_block
      P_SAssignOp id opassign expr  -> undefined
      P_SPostInc id  -> undefined
      P_SPostDec id  -> undefined
      -- pass
      SEmpty  -> return x
      SAssign id expr  -> return x
      SAssignArr id expr1 expr2  -> return x
      SReturn expr  -> return x
      SReturnV  -> return x
      SIf expr stmt -> return $ SIf expr $ transStmt' stmt
      SIfElse expr stmt1 stmt2 -> return $ SIfElse expr (transStmt' stmt1) (transStmt' stmt2)
      SWhile expr stmt -> return $ SWhile expr $ transStmt' stmt
      SForeach type' id expr stmt -> return $ SForeach type' id expr $ transStmt' stmt
      SExpr expr  -> return x
      SThrow expr -> return $ SThrow expr
      STryCatch stmt1 type'2 id3 stmt4 -> return $ STryCatch (transStmt' stmt1) type'2 id3 (transStmt' stmt4)

    transStmt' :: Stmt -> Stmt
    transStmt' x = case transStmt x of
      [stmt] -> stmt
      stmts -> Local [] stmts

    transP_Item :: Type -> P_Item -> [Stmt]
    transP_Item typ x = case x of
      P_NoInit id -> [SDeclVar typ id]
      P_Init id expr -> [SDeclVar typ id, SAssign id expr]

    transP_Block :: P_Block -> Stmt
    transP_Block (P_Block stmts) = Local [] $ do
      stmt <- stmts
      transStmt stmt


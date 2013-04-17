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
transAbs = tP_Prog
  where
    tP_Prog :: P_Prog -> Stmt
    tP_Prog (P_Prog pdeffuncs) = Global $ map tP_DefFunc pdeffuncs

    tP_DefFunc :: P_DefFunc -> Stmt
    tP_DefFunc (P_DefFunc typ id args (P_Excepts excepts) pblock) = SDefFunc typ id args excepts $ tP_Block pblock
    tP_DefFunc (P_DefFunc typ id args (P_NoExcept) pblock) = SDefFunc typ id args [] $ tP_Block pblock

    tStmt :: Stmt -> [Stmt]
    tStmt x = case x of
      -- internal
      Local stmts1 stmts2  -> undefined
      Global stmts  -> undefined
      SDefFunc type' id args types stmt  -> undefined
      SDeclVar type' id  -> undefined
      -- transform
      P_SDeclVar type' p_items  -> concat $ map (tP_Item type') p_items
      P_SBlock p_block  ->  return $ tP_Block p_block
      P_SAssignOp id opassign expr  -> return $ SAssign id $ tExpr $ (case opassign of {
        APlus -> EAdd (EVar id) Plus;
        AMinus -> EAdd (EVar id) Minus;
        ATimes -> EMul (EVar id) Times;
        ADiv -> EMul (EVar id) Div;
        AMod -> EMul (EVar id) Mod; }) (tExpr expr)
      P_SPostInc id  -> tStmt $ P_SAssignOp id APlus (ELitInt 1)
      P_SPostDec id  -> tStmt $ P_SAssignOp id AMinus (ELitInt 1)
      -- pass
      SEmpty  -> return x
      SAssign id expr  -> return $ SAssign id $ tExpr expr
      SAssignArr id expr1 expr2  ->  return $ SAssignArr id (tExpr expr1) (tExpr expr2)
      SReturn expr  -> return $ SReturn $ tExpr expr
      SReturnV  -> return x
      SIf expr stmt -> return $ SIf (tExpr expr) (tStmt' stmt)
      SIfElse expr stmt1 stmt2 -> return $ SIfElse (tExpr expr) (tStmt' stmt1) (tStmt' stmt2)
      SWhile expr stmt -> return $ SWhile (tExpr expr) (tStmt' stmt)
      SForeach type' id expr stmt -> return $ SForeach type' id (tExpr expr) (tStmt' stmt)
      SExpr expr  -> return $ SExpr $ tExpr expr
      SThrow expr -> return $ SThrow $ tExpr expr
      STryCatch stmt1 type'2 id3 stmt4 -> return $ STryCatch (tStmt' stmt1) type'2 id3 (tStmt' stmt4)

    tStmt' :: Stmt -> Stmt
    tStmt' x = case tStmt x of
      [stmt] -> stmt
      stmts -> Local [] stmts

    tP_Item :: Type -> P_Item -> [Stmt]
    tP_Item typ x = case x of
      P_NoInit id -> [SDeclVar typ id]
      P_Init id expr -> [SDeclVar typ id, SAssign id (tExpr expr)]

    tP_Block :: P_Block -> Stmt
    tP_Block (P_Block stmts) = Local [] $ do
      stmt <- stmts
      tStmt stmt

    tExpr :: Expr -> Expr
    tExpr x = case x of
      -- internal
      EBinaryT type' opbin expr1 expr2  -> undefined
      EUnaryT type' opun expr  -> undefined
      -- pass & translate
      EVar id  -> x
      ELitInt n  -> x
      ELitTrue  -> x
      ELitFalse  -> x
      ELitString str  -> x
      ELitChar c  -> x
      ENull  -> x
      EAccessArr expr1 expr2  -> EAccessArr (tExpr expr1) (tExpr expr2)
      EAccessFn expr id exprs  -> EAccessFn (tExpr expr) id (map tExpr exprs)
      EAccessVar expr id  -> EAccessVar (tExpr expr) id
      EApp id exprs  -> EApp id (map tExpr exprs)
      ENewArr type' expr  -> ENewArr type' (tExpr expr)
      ENeg expr  -> unary Neg expr
      ENot expr  -> unary Not expr
      EMul expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      EAdd expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      ERel expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      EAnd expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      EOr expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      where
        unary :: OpUn -> Expr -> Expr
        unary op expr =  EUnaryT TUnknown op (tExpr expr)
        binary :: OpBin -> Expr -> Expr -> Expr
        binary op expr1 expr2 = EBinaryT TUnknown op (tExpr expr1) (tExpr expr2)


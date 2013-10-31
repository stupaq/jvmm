module Semantics.Trans where
import Prelude hiding (id)
import Syntax.AbsJvmm as I
import Semantics.APTree as O

identToString :: I.Ident -> String
identToString (I.Ident id) = id

-- Creates variable-associated identifier from given one (for temporary and
-- iteration variables). Only variable-associated identifiers derived from the
-- same variable and with the same context will be hidden by the created one.
tempIdent :: I.Ident -> String -> I.Ident
tempIdent (I.Ident id) ctx = I.Ident $ id ++ "#" ++ ctx

-- Performs abstract syntax tree transformations that do not alter semantics
-- and simplify its structure. After running translate on AST one can assume
-- non-existance of all labels prefixed with "P_" (type separation might be
-- introduced in the future). This is always a good place to remove unnecessary
-- syntactic sugar.
transAbs :: P_Prog -> I.Stmt
transAbs = tP_Prog
  where
    unary :: I.OpUn -> I.Expr -> I.Expr
    unary op expr =  I.EUnaryT I.TUnknown op (tExpr expr)
    binary :: I.OpBin -> I.Expr -> I.Expr -> I.Expr
    binary op expr1 expr2 = I.EBinaryT I.TUnknown op (tExpr expr1) (tExpr expr2)

    tP_Prog :: P_Prog -> I.Stmt
    tP_Prog (P_Prog p_defglobals) = Global $ map tP_DefGlobal p_defglobals

    tP_DefGlobal :: P_DefGlobal -> I.Stmt
    tP_DefGlobal x = case x of
      P_GlobFunc p_deffunc  -> tP_DefFunc p_deffunc
      P_GlobClass p_defclass  -> tP_DefClass p_defclass

    tP_DefClass :: P_DefClass -> I.Stmt
    tP_DefClass x = case x of
      P_DefClass id p_members  -> I.SDefClass id $ Global $ map tP_Member p_members

    tP_Member :: P_Member -> I.Stmt
    tP_Member x = case x of
      P_Field type' id  -> I.SDeclVar type' id

    tP_DefFunc :: P_DefFunc -> I.Stmt
    tP_DefFunc (P_DefFunc typ id args (P_Excepts excepts) pblock) = I.SDefFunc typ id (tP_Args args) excepts $ tP_Block pblock
    tP_DefFunc (P_DefFunc typ id args (P_NoExcept) pblock) = I.SDefFunc typ id (tP_Args args) [] $ tP_Block pblock

    tStmt :: I.Stmt -> [I.Stmt]
    tStmt x = case x of
      -- internal
      Local stmts1 stmts2  -> undefined
      Global stmts  -> undefined
      I.SDefClass id stmt  -> undefined
      I.SDefFunc type' id args types stmt  -> undefined
      I.SDeclVar type' id  -> undefined
      -- transform
      P_SDeclVar type' p_items  -> concat $ map (tP_Item type') p_items
      P_SBlock p_block  ->  return $ tP_Block p_block
      P_SAssignOp id opassign expr  -> return $ I.SAssign id $ tExpr $ tAssignOp opassign (I.EVar id) expr
      P_SAssignOpArr id expr1 opassign2 expr3  ->  return $ I.SAssignArr id (tExpr expr1) $ tExpr $ tAssignOp opassign2 (I.EAccessArr (I.EVar id) expr1) expr3
      P_SAssignOpFld id1 id2 opassign3 expr4  ->  return $ I.SAssignFld id1 id2 $ tExpr $ tAssignOp opassign3 (I.EAccessVar (I.EVar id1) id2) expr4
      P_SPostInc id  -> tStmt $ P_SAssignOp id APlus (I.ELitInt 1)
      P_SPostDec id  -> tStmt $ P_SAssignOp id AMinus (I.ELitInt 1)
      -- pass
      I.SEmpty  -> return x
      I.SAssign id expr  -> return $ I.SAssign id $ tExpr expr
      I.SAssignArr id expr1 expr2  ->  return $ I.SAssignArr id (tExpr expr1) (tExpr expr2)
      I.SAssignFld id1 id2 expr3  -> return $ I.SAssignFld id1 id2 (tExpr expr3)
      I.SReturn expr  -> return $ I.SReturn $ tExpr expr
      I.SReturnV  -> return x
      I.SIf expr stmt -> return $ I.SIf (tExpr expr) (tStmt' stmt)
      I.SIfElse expr stmt1 stmt2 -> return $ I.SIfElse (tExpr expr) (tStmt' stmt1) (tStmt' stmt2)
      I.SWhile expr stmt -> return $ I.SWhile (tExpr expr) (tStmt' stmt)
      P_SForeach type' id expr stmt -> -- SYNTACTIC SUGAR
        let idarr = tempIdent id "arr"
            idlength = tempIdent id "length"
            iditer = tempIdent id "iter"
        in return $ tP_Block $ P_Block $ [
          P_SDeclVar (I.TArray I.TInt) [P_Init idarr expr],
          P_SDeclVar I.TInt [P_Init idlength (I.EAccessVar (I.EVar idarr) (I.Ident "length")), P_Init iditer (I.ELitInt 0)],
          I.SWhile (P_ERel (I.EVar iditer) LTH (I.EVar idlength)) $ P_SBlock $ P_Block [
            P_SDeclVar type' [P_Init id (I.EAccessArr (I.EVar idarr) (I.EVar iditer))],
            stmt,
            P_SPostInc iditer]]
      I.SExpr expr  -> return $ I.SExpr $ tExpr expr
      I.SThrow expr -> return $ I.SThrow $ tExpr expr
      I.STryCatch stmt1 type'2 id3 stmt4 -> return $ I.STryCatch (tStmt' stmt1) type'2 id3 (tStmt' stmt4)
      where
      tAssignOp :: OpAssign -> I.Expr -> I.Expr -> I.Expr
      tAssignOp opassign expr1 expr2 = case opassign of
        APlus -> P_EAdd expr1 Plus expr2
        AMinus -> P_EAdd expr1 Minus expr2
        ATimes -> P_EMul expr1 I.Times expr2
        ADiv -> P_EMul expr1 Div expr2
        AMod -> P_EMul expr1 Mod expr2

    tStmt' :: I.Stmt -> I.Stmt
    tStmt' x = case tStmt x of
      [stmt] -> stmt
      stmts -> Local [] stmts

    tP_Args :: [P_Arg] -> [I.Stmt]
    tP_Args = map (\(P_Arg typ id) -> I.SDeclVar typ id)

    tP_Item :: I.Type -> P_Item -> [I.Stmt]
    tP_Item typ x = case x of
      P_NoInit id -> [I.SDeclVar typ id]
      -- We use temporary variable with lifetime limited to four statements,
      -- which cannot hide any user-defined variable
      P_Init id expr -> -- SYNTACTIC SUGAR
        let idtmp = tempIdent id "decl"
        in [I.SDeclVar typ idtmp, I.SAssign idtmp (tExpr expr), I.SDeclVar typ id, I.SAssign id (I.EVar idtmp)]

    tP_Block :: P_Block -> I.Stmt
    tP_Block (P_Block stmts) = Local [] $ do
      stmt <- stmts
      tStmt stmt

    tExpr :: I.Expr -> I.Expr
    tExpr x = case x of
      -- internal
      I.EBinaryT type' opbin expr1 expr2  -> undefined
      I.EUnaryT type' opun expr  -> undefined
      -- pass
      I.EVar id  -> x
      I.ELitInt n  -> x
      I.ELitTrue  -> x
      I.ELitFalse  -> x
      I.ELitString str  -> x
      I.ELitChar c  -> x
      I.ENull  -> x
      I.EAccessArr expr1 expr2  -> I.EAccessArr (tExpr expr1) (tExpr expr2)
      I.EAccessFn expr id exprs  -> I.EAccessFn (tExpr expr) id (map tExpr exprs)
      I.EAccessVar expr id  -> I.EAccessVar (tExpr expr) id
      I.EApp id exprs  -> I.EApp id (map tExpr exprs)
      I.ENewArr type' expr  -> I.ENewArr type' (tExpr expr)
      I.ENewObj type'  -> x
      -- translate
      P_ENeg expr  -> unary Neg expr
      P_ENot expr  -> unary Not expr
      P_EMul expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      P_EAdd expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      P_ERel expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      P_EAnd expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      P_EOr expr1 opbin2 expr3  -> binary opbin2 expr1 expr3


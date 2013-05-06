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

-- Creates variable-associated identifier from given one (for temporary and
-- iteration variables). Only variable-associated identifiers drived from the
-- same variable and with the same context will be hidden by the created one.
tempIdent :: Ident -> String -> Ident
tempIdent (Ident id) ctx = Ident $ id ++ "#" ++ ctx

-- Performs abstract syntax tree transformations that do not alter semantics
-- and simplify its structure. After running translate on AST one can assume
-- non-existance of all labels prefixed with "P_" (type separation might be
-- introduced in the future). This is always a good place to remove unnecessary
-- syntactic sugar.
transAbs :: P_Prog -> Stmt
transAbs = tP_Prog
  where
    unary :: OpUn -> Expr -> Expr
    unary op expr =  EUnaryT TUnknown op (tExpr expr)
    binary :: OpBin -> Expr -> Expr -> Expr
    binary op expr1 expr2 = EBinaryT TUnknown op (tExpr expr1) (tExpr expr2)

    tP_Prog :: P_Prog -> Stmt
    tP_Prog (P_Prog p_defglobals) = Global $ map tP_DefGlobal p_defglobals

    tP_DefGlobal :: P_DefGlobal -> Stmt
    tP_DefGlobal x = case x of
      P_GlobFunc p_deffunc  -> tP_DefFunc p_deffunc
      P_GlobClass p_defclass  -> tP_DefClass p_defclass

    tP_DefClass :: P_DefClass -> Stmt
    tP_DefClass x = case x of
      P_DefClass id p_members  -> SDefClass id $ Global $ map tP_Member p_members

    tP_Member :: P_Member -> Stmt
    tP_Member x = case x of
      P_Field type' id  -> SDeclVar type' id

    tP_DefFunc :: P_DefFunc -> Stmt
    tP_DefFunc (P_DefFunc typ id args (P_Excepts excepts) pblock) = SDefFunc typ id (tP_Args args) excepts $ tP_Block pblock
    tP_DefFunc (P_DefFunc typ id args (P_NoExcept) pblock) = SDefFunc typ id (tP_Args args) [] $ tP_Block pblock

    tStmt :: Stmt -> [Stmt]
    tStmt x = case x of
      -- internal
      Local stmts1 stmts2  -> undefined
      Global stmts  -> undefined
      SDefClass id stmt  -> undefined
      SDefFunc type' id args types stmt  -> undefined
      SDeclVar type' id  -> undefined
      -- transform
      P_SDeclVar type' p_items  -> concat $ map (tP_Item type') p_items
      P_SBlock p_block  ->  return $ tP_Block p_block
      P_SAssignOp id opassign expr  -> return $ SAssign id $ tExpr $ (case opassign of {
        APlus -> P_EAdd (EVar id) Plus;
        AMinus -> P_EAdd (EVar id) Minus;
        ATimes -> P_EMul (EVar id) Times;
        ADiv -> P_EMul (EVar id) Div;
        AMod -> P_EMul (EVar id) Mod; }) (tExpr expr)
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
      P_SForeach type' id expr stmt -> -- SYNTACTIC SUGAR
        let idarr = tempIdent id "arr"
            idlength = tempIdent id "length"
            iditer = tempIdent id "iter"
        in return $ tP_Block $ P_Block $ [
          P_SDeclVar (TArray TInt) [P_Init idarr expr],
          P_SDeclVar TInt [P_Init idlength (EAccessVar (EVar idarr) (Ident "length")), P_Init iditer (ELitInt 0)],
          SWhile (P_ERel (EVar iditer) LTH (EVar idlength)) $ P_SBlock $ P_Block [
            P_SDeclVar type' [P_Init id (EAccessArr (EVar idarr) (EVar iditer))],
            stmt,
            P_SPostInc iditer]]
      SExpr expr  -> return $ SExpr $ tExpr expr
      SThrow expr -> return $ SThrow $ tExpr expr
      STryCatch stmt1 type'2 id3 stmt4 -> return $ STryCatch (tStmt' stmt1) type'2 id3 (tStmt' stmt4)

    tStmt' :: Stmt -> Stmt
    tStmt' x = case tStmt x of
      [stmt] -> stmt
      stmts -> Local [] stmts

    tP_Args :: [P_Arg] -> [Stmt]
    tP_Args = map (\(P_Arg typ id) -> SDeclVar typ id)

    tP_Item :: Type -> P_Item -> [Stmt]
    tP_Item typ x = case x of
      P_NoInit id -> [SDeclVar typ id]
      -- We use temporary variable with lifetime limited to four statements,
      -- which cannot hide any user-defined variable
      P_Init id expr -> -- SYNTACTIC SUGAR
        let idtmp = tempIdent id "decl"
        in [SDeclVar typ idtmp, SAssign idtmp (tExpr expr), SDeclVar typ id, SAssign id (EVar idtmp)]

    tP_Block :: P_Block -> Stmt
    tP_Block (P_Block stmts) = Local [] $ do
      stmt <- stmts
      tStmt stmt

    tExpr :: Expr -> Expr
    tExpr x = case x of
      -- internal
      EBinaryT type' opbin expr1 expr2  -> undefined
      EUnaryT type' opun expr  -> undefined
      -- pass
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
      -- translate
      P_ENeg expr  -> unary Neg expr
      P_ENot expr  -> unary Not expr
      P_EMul expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      P_EAdd expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      P_ERel expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      P_EAnd expr1 opbin2 expr3  -> binary opbin2 expr1 expr3
      P_EOr expr1 opbin2 expr3  -> binary opbin2 expr1 expr3


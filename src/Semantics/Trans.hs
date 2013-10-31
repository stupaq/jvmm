module Semantics.Trans where
import Prelude hiding (id)
import Syntax.AbsJvmm as I
import Semantics.APTree as O

-- FIXME run translation on syntactic sugar (fixpoint)

-- TODO remove
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
transAbs :: P_Prog -> O.Stmt
transAbs = tP_Prog
  where
    tP_Prog :: P_Prog -> O.Stmt
    tP_Prog (P_Prog p_defglobals) = O.SGlobal $ map tP_DefGlobal p_defglobals

    tP_DefGlobal :: P_DefGlobal -> O.Stmt
    tP_DefGlobal x = case x of
      P_GlobFunc p_deffunc  -> tP_DefFunc p_deffunc
      P_GlobClass p_defclass  -> tP_DefClass p_defclass

    tP_DefClass :: P_DefClass -> O.Stmt
    tP_DefClass x = case x of
      P_DefClass id p_members  -> O.SDefClass (tTIdent id) $ O.SGlobal $ map tP_Member p_members

    tP_Member :: P_Member -> O.Stmt
    tP_Member x = case x of
      P_Field typ id  -> O.SDeclVar (tType typ) (tVIdent id)

    tP_DefFunc :: P_DefFunc -> O.Stmt
    tP_DefFunc (P_DefFunc typ id args (P_Excepts excepts) pblock) = O.SDefFunc (tType typ) (tFIdent id) (tP_Args args) (map tType excepts) $ tP_Block pblock
    tP_DefFunc (P_DefFunc typ id args (P_NoExcept) pblock) = O.SDefFunc (tType typ) (tFIdent id) (tP_Args args) [] $ tP_Block pblock

    tStmt :: I.Stmt -> [O.Stmt]
    tStmt x = case x of
      -- internal
      I.Local stmts1 stmts2  -> undefined
      I.Global stmts  -> undefined
      I.SDefClass id stmt  -> undefined
      I.SDefFunc typ id args types stmt  -> undefined
      I.SDeclVar typ id  -> undefined
      -- transform
      P_SDeclVar typ p_items  -> concat $ map (tP_Item typ) p_items
      P_SBlock p_block  ->  return $ tP_Block p_block
      P_SAssignOp id opassign expr  -> return $ O.SAssign (tVIdent id) $ tExpr $ tAssignOp opassign (I.EVar id) expr
      P_SAssignOpArr id expr1 opassign2 expr3  ->  return $ O.SAssignArr (tVIdent id) (tExpr expr1) $ tExpr $ tAssignOp opassign2 (I.EAccessArr (I.EVar id) expr1) expr3
      P_SAssignOpFld id1 id2 opassign3 expr4  ->  return $ O.SAssignFld (tVIdent id1) (tVIdent id2) $ tExpr $ tAssignOp opassign3 (I.EAccessVar (I.EVar id1) id2) expr4
      P_SPostInc id  -> tStmt $ P_SAssignOp id APlus (I.ELitInt 1)
      P_SPostDec id  -> tStmt $ P_SAssignOp id AMinus (I.ELitInt 1)
      -- pass
      I.SEmpty  -> return O.SEmpty
      I.SAssign id expr  -> return $ O.SAssign (tVIdent id) $ tExpr expr
      I.SAssignArr id expr1 expr2  ->  return $ O.SAssignArr (tVIdent id) (tExpr expr1) (tExpr expr2)
      I.SAssignFld id1 id2 expr3  -> return $ O.SAssignFld (tVIdent id1) (tVIdent id2) (tExpr expr3)
      I.SReturn expr  -> return $ O.SReturn $ tExpr expr
      I.SReturnV  -> return O.SReturnV
      I.SIf expr stmt -> return $ O.SIf (tExpr expr) (tStmt' stmt)
      I.SIfElse expr stmt1 stmt2 -> return $ O.SIfElse (tExpr expr) (tStmt' stmt1) (tStmt' stmt2)
      I.SWhile expr stmt -> return $ O.SWhile (tExpr expr) (tStmt' stmt)
      P_SForeach typ id expr stmt -> -- SYNTACTIC SUGAR
        let idarr = tempIdent id "arr"
            idlength = tempIdent id "length"
            iditer = tempIdent id "iter"
        in return $ tP_Block $ P_Block $ [
          P_SDeclVar (I.TArray I.TInt) [P_Init idarr expr],
          P_SDeclVar I.TInt [P_Init idlength (I.EAccessVar (I.EVar idarr) (I.Ident "length")), P_Init iditer (I.ELitInt 0)],
          I.SWhile (P_ERel (I.EVar iditer) LTH (I.EVar idlength)) $ P_SBlock $ P_Block [
            P_SDeclVar typ [P_Init id (I.EAccessArr (I.EVar idarr) (I.EVar iditer))],
            stmt,
            P_SPostInc iditer]]
      I.SExpr expr  -> return $ O.SExpr $ tExpr expr
      I.SThrow expr -> return $ O.SThrow $ tExpr expr
      I.STryCatch stmt1 typ2 id3 stmt4 -> return $ O.STryCatch (tStmt' stmt1) (tType typ2) (tVIdent id3) (tStmt' stmt4)
      where
      tAssignOp :: OpAssign -> I.Expr -> I.Expr -> I.Expr
      tAssignOp opassign expr1 expr2 = case opassign of
        APlus -> P_EAdd expr1 Plus expr2
        AMinus -> P_EAdd expr1 Minus expr2
        ATimes -> P_EMul expr1 I.Times expr2
        ADiv -> P_EMul expr1 Div expr2
        AMod -> P_EMul expr1 Mod expr2

    tStmt' :: I.Stmt -> O.Stmt
    tStmt' x = case tStmt x of
      [stmt] -> stmt
      stmts -> O.SLocal [] stmts

    tP_Args :: [P_Arg] -> [O.Stmt]
    tP_Args = map (\(P_Arg typ id) -> O.SDeclVar (tType typ) (tVIdent id))

    tP_Item :: I.Type -> P_Item -> [O.Stmt]
    tP_Item typ x = case x of
      P_NoInit id -> [O.SDeclVar (tType typ) (tVIdent id)]
      -- We use temporary variable with lifetime limited to four statements,
      -- which cannot hide any user-defined variable
      P_Init id expr -> -- SYNTACTIC SUGAR
        let idtmp = tempIdent id "decl"
        in [O.SDeclVar (tType typ) (tVIdent idtmp), O.SAssign (tVIdent idtmp) (tExpr expr), O.SDeclVar (tType typ) (tVIdent id), O.SAssign (tVIdent id) (O.EVar (tVIdent idtmp))]

    tP_Block :: P_Block -> O.Stmt
    tP_Block (P_Block stmts) = O.SLocal [] $ do
      stmt <- stmts
      tStmt stmt

    tExpr :: I.Expr -> O.Expr
    tExpr x = case x of
      -- internal
      I.EBinaryT typ opbin expr1 expr2  -> undefined
      I.EUnaryT typ opun expr  -> undefined
      -- pass
      I.EVar id  -> O.EVar $ tVIdent id
      I.ELitInt n  -> O.ELitInt n
      I.ELitTrue  -> O.ELitTrue
      I.ELitFalse  -> O.ELitFalse
      I.ELitString str  -> O.ELitString str
      I.ELitChar c  -> O.ELitChar c
      I.ENull  -> O.ENull
      I.EAccessArr expr1 expr2  -> O.EAccessArr (tExpr expr1) (tExpr expr2)
      I.EAccessFn expr id exprs  -> O.EAccessFn (tExpr expr) (tFIdent id) (map tExpr exprs)
      I.EAccessVar expr id  -> O.EAccessVar (tExpr expr) (tVIdent id)
      I.EApp id exprs  -> O.EApp (tFIdent id) (map tExpr exprs)
      I.ENewArr typ expr  -> O.ENewArr (tType typ) (tExpr expr)
      I.ENewObj typ  -> O.ENewObj $ tType typ
      -- translate
      P_ENeg expr  -> O.EUnary O.TUnknown O.OuNeg (tExpr expr)
      P_ENot expr  -> O.EUnary O.TUnknown O.OuNot (tExpr expr)
      P_EMul expr1 opbin2 expr3  -> bin opbin2 expr1 expr3
      P_EAdd expr1 opbin2 expr3  -> bin opbin2 expr1 expr3
      P_ERel expr1 opbin2 expr3  -> bin opbin2 expr1 expr3
      P_EAnd expr1 opbin2 expr3  -> bin opbin2 expr1 expr3
      P_EOr expr1 opbin2 expr3  -> bin opbin2 expr1 expr3
      where
        bin :: I.OpBin -> I.Expr -> I.Expr -> O.Expr
        bin op expr1 expr2 = O.EBinary O.TUnknown (tOpBin op) (tExpr expr1) (tExpr expr2)

    tOpBin :: I.OpBin -> O.OpBin
    tOpBin x = case x of
      I.Times  -> O.ObTimes
      I.Div  -> O.ObDiv
      I.Mod  -> O.ObMod
      I.Plus  -> O.ObPlus
      I.Minus  -> O.ObMinus
      I.LTH  -> O.ObLTH
      I.LEQ  -> O.ObLEQ
      I.GTH  -> O.ObGTH
      I.GEQ  -> O.ObGEQ
      I.EQU  -> O.ObEQU
      I.NEQ  -> O.ObNEQ
      I.And  -> O.ObAnd
      I.Or  -> O.ObOr

    tType :: I.Type -> O.Type
    tType x = case x of
      I.TFunc typ types1 types2 -> undefined
      I.TUnknown -> O.TUnknown
      I.TNull -> undefined
      I.TVoid -> O.TVoid
      I.TInt -> O.TInt
      I.TChar -> O.TChar
      I.TBool -> O.TBool
      I.TString -> O.TString
      I.TUser id -> O.TUser (tTIdent id)
      I.TObject -> O.TObject
      I.TArray typ -> O.TArray $ tType typ

    tVIdent, tFIdent, tTIdent :: I.Ident -> O.UIdent
    tVIdent (Ident id) = VIdent id
    tFIdent (Ident id) = FIdent id
    tTIdent (Ident id) = TIdent id


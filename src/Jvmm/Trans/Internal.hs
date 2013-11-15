module Jvmm.Trans.Internal where
import qualified Jvmm.Trans.Output as O

import Control.Monad.Identity

import qualified Syntax.AbsJvmm as I

import Jvmm.Builtins (buildObjectClass)
import Jvmm.Hierarchy (prepareClassDiff)

-- Creates variable-associated identifier from given one (for temporary and iteration variables).
-- Only variable-associated identifiers derived from the same variable and with the same context
-- will be hidden by the created one.
tempIdent :: I.Ident -> String -> I.Ident
tempIdent (I.Ident id) ctx = I.Ident $ id ++ "#" ++ ctx

tProgram :: I.Program -> O.CompilationUnit
tProgram (I.Program defs) = O.CompilationUnit $
  let userClasses = [ x | I.DClass x <- defs ]
      objectClass = buildObjectClass [ tFunction x | I.DFunction x <- defs ]
  in (O.TUnknown, prepareClassDiff objectClass) : map tClass userClasses

tClass :: I.Class -> (O.Type, O.ClassDiff)
tClass (I.Class id extends members) =
  let typ = O.TUser (tTIdent id)
      super = tExtends extends
  in (super, prepareClassDiff $ O.Class {
      O.classType = typ,
      O.classSuper = super,
      O.classFields = [ (tDeclaration x) { O.fieldOrigin = typ } | I.Field x <- members ],
      O.classMethods = [ (tFunction x) { O.methodOrigin = typ } | I.Method x <- members ]
    })

tExtends :: I.Extends -> O.Type
tExtends x = case x of
  I.SuperClass typ -> tType typ
  I.SuperObject -> tType I.TObject

tDeclaration :: I.Declaration -> O.Field
tDeclaration (I.DVariable typ id) = O.Field {
  O.fieldType = tType typ,
  O.fieldIdent = tVIdent id,
  O.fieldOrigin = O.TUnknown
}

tFunction :: I.Function -> O.Method
tFunction (I.Function typ id args exceptions stmts) =
  O.Method {
    O.methodType = funType,
    O.methodIdent = tFIdent id,
    O.methodArgs = argUIdents,
    O.methodBody = tStmts stmts,
    O.methodOrigin = O.TUnknown
  }
  where
    argUIdents = map (\(I.Argument typ id) -> tVIdent id) args
    argTypes = map (\(I.Argument typ id) -> tType typ) args
    funType = O.TFunc (tType typ) argTypes exceptionTypes
    exceptionTypes = case exceptions of
      I.Exceptions elist -> map tType elist
      _ -> []

tStmt :: I.Stmt -> [O.Stmt]
tStmt x = case x of
  I.SDeclVar typ items -> concat $ map (tItem typ) items
  I.SBlock stmts ->  return $ tStmts stmts
  I.SAssignOp id opassign expr -> return $ O.SAssign (tVIdent id) $ tExpr $ tAssignOp opassign (I.EVar id) expr
  I.SAssignOpArr id expr1 opassign2 expr3 ->  return $ O.SAssignArr (tVIdent id) (tExpr expr1) $ tExpr $ tAssignOp opassign2 (I.EArrayE (I.EVar id) expr1) expr3
  I.SAssignOpFld id1 id2 opassign3 expr4 ->  return $ O.SAssignFld (tVIdent id1) (tVIdent id2) $ tExpr $ tAssignOp opassign3 (I.EFieldE (I.EVar id1) id2) expr4
  I.SAssignOpThis id2 opassign3 expr4 -> return $ O.SAssignFld O.IThis (tVIdent id2) $ tExpr $ tAssignOp opassign3 (I.EFieldE I.EThis id2) expr4
  I.SPostInc id -> tStmt $ I.SAssignOp id I.APlus (I.ELitInt 1)
  I.SPostDec id -> tStmt $ I.SAssignOp id I.AMinus (I.ELitInt 1)
  I.SEmpty -> return O.SEmpty
  I.SAssign id expr -> return $ O.SAssign (tVIdent id) $ tExpr expr
  I.SAssignArr id expr1 expr2 ->  return $ O.SAssignArr (tVIdent id) (tExpr expr1) (tExpr expr2)
  I.SAssignFld id1 id2 expr3 -> return $ O.SAssignFld (tVIdent id1) (tVIdent id2) (tExpr expr3)
  I.SAssignThis id2 expr3 -> return $ O.SAssignFld O.IThis (tVIdent id2) (tExpr expr3)
  I.SReturn expr -> return $ O.SReturn $ tExpr expr
  I.SReturnV -> return O.SReturnV
  I.SIf expr stmt -> return $ O.SIf (tExpr expr) (tStmt' stmt)
  I.SIfElse expr stmt1 stmt2 -> return $ O.SIfElse (tExpr expr) (tStmt' stmt1) (tStmt' stmt2)
  I.SWhile expr stmt -> return $ O.SWhile (tExpr expr) (tStmt' stmt)
  I.SForeach typ id expr stmt ->  -- SYNTACTIC SUGAR
    let idarr = tempIdent id "arr"
        idlength = tempIdent id "length"
        iditer = tempIdent id "iter"
    in tStmt $ I.SBlock $ [
      I.SDeclVar (I.TArray I.TInt) [I.Init idarr expr],
      I.SDeclVar I.TInt [I.Init idlength (I.EFieldE (I.EVar idarr) (I.Ident "length")),
      I.Init iditer (I.ELitInt 0)],
      I.SWhile (I.ERel (I.EVar iditer) I.LTH (I.EVar idlength)) $ I.SBlock [
        I.SDeclVar typ [I.Init id (I.EArrayE (I.EVar idarr) (I.EVar iditer))],
        stmt,
        I.SPostInc iditer]]
  I.SExpr expr -> return $ O.SExpr $ tExpr expr
  I.SThrow expr -> return $ O.SThrow $ tExpr expr
  I.STryCatch stmt1 typ2 id3 stmt4 -> return $ O.STryCatch (tStmt' stmt1) (tType typ2) (tVIdent id3) (tStmt' stmt4)
  where
  tAssignOp :: I.AssignOp -> I.Expr -> I.Expr -> I.Expr
  tAssignOp opassign expr1 expr2 = case opassign of
    I.APlus -> I.EAdd expr1 I.Plus expr2
    I.AMinus -> I.EAdd expr1 I.Minus expr2
    I.ATimes -> I.EMul expr1 I.Times expr2
    I.ADiv -> I.EMul expr1 I.Div expr2
    I.AMod -> I.EMul expr1 I.Mod expr2

tStmt' :: I.Stmt -> O.Stmt
tStmt' x = case tStmt x of
  [stmt] -> stmt
  stmts -> O.SLocal [] stmts

tStmts :: [I.Stmt] -> O.Stmt
tStmts stmts = O.SLocal [] $ do
  stmt <- stmts
  tStmt stmt

tItem :: I.Type -> I.Item -> [O.Stmt]
tItem typ x = case x of
  I.NoInit id -> [O.SDeclVar (tType typ) (tVIdent id)]
  -- We use temporary variable with lifetime limited to four statements,
  -- which cannot hide any user-defined variable
  I.Init id expr ->  -- SYNTACTIC SUGAR
    let idtmp = tempIdent id "decl"
    in [
      O.SDeclVar (tType typ) (tVIdent idtmp),
      O.SAssign (tVIdent idtmp) (tExpr expr),
      O.SDeclVar (tType typ) (tVIdent id),
      O.SAssign (tVIdent id) (O.EVar (tVIdent idtmp))]

tExpr :: I.Expr -> O.Expr
tExpr x = case x of
  I.EVar id -> O.EVar $ tVIdent id
  I.ELitInt n -> O.ELitInt n
  I.ELitTrue -> O.ELitTrue
  I.ELitFalse -> O.ELitFalse
  I.EString str -> O.ELitString str
  I.ELitChar c -> O.ELitChar c
  I.ENull -> O.ENull
  I.ENullT typ -> O.ENull
  I.EArrayE expr1 expr2 -> O.EAccessArr (tExpr expr1) (tExpr expr2)
  I.EMethodE expr id exprs -> O.EAccessFn (tExpr expr) (tFIdent id) (map tExpr exprs)
  I.EFieldE expr id -> O.EAccessVar (tExpr expr) (tVIdent id)
  I.EArrayI ide expr2 -> O.EAccessArr (tExpr $ I.EVar ide) (tExpr expr2)  -- GRAMMAR IRREGULARITY
  I.EMethodI ide id exprs -> O.EAccessFn (tExpr $ I.EVar ide) (tFIdent id) (map tExpr exprs)  -- GRAMMAR IRREGULARITY
  I.EFieldI ide id -> O.EAccessVar (tExpr $ I.EVar ide) (tVIdent id)  -- GRAMMAR IRREGULARITY
  I.EFieldIT id -> O.EAccessVar O.EThis (tVIdent id)
  I.EMethodIT id exprs -> O.EAccessFn O.EThis (tFIdent id) (map tExpr exprs)
  I.EApp id exprs -> O.EAccessFn O.EThis (tFIdent id) (map tExpr exprs)
  I.ENewArray typ expr -> O.ENewArr (tType typ) (tExpr expr)
  I.ENewObject typ -> O.ENewObj $ tType typ
  I.ENeg expr -> O.EUnary O.TUnknown O.OuNeg (tExpr expr)
  I.ENot expr -> O.EUnary O.TUnknown O.OuNot (tExpr expr)
  I.EMul expr1 opbin2 expr3 -> O.EBinary O.TUnknown (tMulOp opbin2) (tExpr expr1) (tExpr expr3)
  I.EAdd expr1 opbin2 expr3 -> O.EBinary O.TUnknown (tAddOp opbin2) (tExpr expr1) (tExpr expr3)
  I.ERel expr1 opbin2 expr3 -> O.EBinary O.TUnknown (tRelOp opbin2) (tExpr expr1) (tExpr expr3)
  I.EAnd expr1 expr3 -> O.EBinary O.TUnknown O.ObAnd (tExpr expr1) (tExpr expr3)
  I.EOr expr1 expr3 -> O.EBinary O.TUnknown O.ObOr (tExpr expr1) (tExpr expr3)
  I.EThis -> O.EThis

tMulOp :: I.MulOp -> O.OpBin
tMulOp x = case x of
  I.Times -> O.ObTimes
  I.Div -> O.ObDiv
  I.Mod -> O.ObMod

tAddOp :: I.AddOp -> O.OpBin
tAddOp x = case x of
  I.Plus -> O.ObPlus
  I.Minus -> O.ObMinus

tRelOp :: I.RelOp -> O.OpBin
tRelOp x = case x of
  I.LTH -> O.ObLTH
  I.LEQ -> O.ObLEQ
  I.GTH -> O.ObGTH
  I.GEQ -> O.ObGEQ
  I.EQU -> O.ObEQU
  I.NEQ -> O.ObNEQ

tType :: I.Type -> O.Type
tType x = case x of
  I.TVoid -> O.TVoid
  I.TInt -> O.TInt
  I.TChar -> O.TChar
  I.TBool -> O.TBool
  I.TString -> O.TString
  I.TUser id -> O.TUser (tTIdent id)
  I.TObject -> O.TObject
  I.TArray typ -> O.TArray $ tType typ

tVIdent, tFIdent, tTIdent :: I.Ident -> O.UIdent
tVIdent (I.Ident id) = O.VIdent id
tFIdent (I.Ident id) = O.FIdent id
tTIdent (I.Ident id) = O.TIdent id


module Jvmm.Trans.Internal where
import qualified Jvmm.Trans.Output as O

import Control.Monad.Identity

import qualified Syntax.AbsJvmm as I

import qualified Jvmm.Errors as Err
import Jvmm.Errors (ErrorInfoT, Location)
import Jvmm.Hierarchy (prepareClassDiff, objectClassDiff)

-- Creates variable-associated identifier from given one (for temporary and iteration variables).
-- Only variable-associated identifiers derived from the same variable and with the same context
-- will be hidden by the created one.
tempIdent :: I.Ident -> String -> I.Ident
tempIdent (I.Ident id) ctx = I.Ident $ id ++ "#" ++ ctx

tProgram :: I.Program -> ErrorInfoT Identity O.CompilationUnit
tProgram (I.Program defs) = do
  objectDiff <- objectClassDiff [ tFunction x | I.DFunction x <- defs ]
  let userClasses = [ x | I.DClass x <- defs ]
  return $ O.CompilationUnit $ (undefined, objectDiff):map tClass userClasses

tClass :: I.Class -> (O.TypeComposed, O.ClassDiff)
tClass (I.Class id extends lbr members rbr) =
  let clazzTyp = O.TUser (tTIdent id)
      super = tExtends extends
  in (super, prepareClassDiff (brToLoc lbr rbr) $ O.Class {
        O.classType = clazzTyp
      , O.classSuper = super
      , O.classFields = [ tField typ x | I.FieldsList typ fields _ <- members, x <- fields ]
      , O.classMethods = [ tFunction x | I.Method x <- members ]
      , O.classStaticMethods = []
      , O.classLocation = Err.Unknown
    })

tExtends :: I.Extends -> O.TypeComposed
tExtends x = case x of
  I.SuperClass typ -> tTComposed typ
  I.SuperObject -> tTComposed I.TObject

tField :: I.TypeBasic -> I.Field -> O.Field
tField typ (I.Field id) = O.Field {
  O.fieldType = tTBasic typ,
  O.fieldName = tFIdent id,
  O.fieldOrigin = undefined
}

tFunction :: I.Function -> O.Method
tFunction (I.Function typ id args exceptions lbr stmts rbr) =
  O.Method {
      O.methodType = funType
    , O.methodName = tMIdent id
    , O.methodArgs = arguments
    , O.methodBody = O.SBlock $ tBraces lbr rbr (concatMap tStmt stmts)
    , O.methodOrigin = undefined
    , O.methodLocation = brToLoc lbr rbr
    , O.methodVariables = []
  }
  where
    arguments = map (\(I.Argument typ id) -> O.Variable (tTBasic typ) undefined (tVIdent id))  args
    argTypes = map (\(I.Argument typ id) -> tTBasic typ) args
    funType = O.TypeMethod (tTBasic typ) argTypes exceptionTypes
    exceptionTypes = case exceptions of
      I.Exceptions elist -> map tTComposed elist
      _ -> []

tStmt :: I.Stmt -> [O.Stmt]
tStmt x = case x of
  I.SDeclVar typ items s -> tSem s $ concat $ map (tItem typ) items
  I.SBlock lbr stmts rbr -> tBraces lbr rbr $ return $
    O.SBlock $ concatMap tStmt stmts
  I.SAssignOp id opassign expr s -> tSem s $ return $
    O.T_SAssign (tVIdent id) $ tExpr $ tAssignOp opassign (I.EVar id) expr
  I.SAssignOpArr id expr1 opassign2 expr3 s -> tSem s $ return $
    O.T_SAssignArr (tVIdent id) (tExpr expr1) $ tExpr $ tAssignOp opassign2 (I.EArrayE (I.EVar id) expr1) expr3
  I.SAssignOpFld id1 id2 opassign3 expr4 s -> tSem s $ return $
    O.T_SAssignFld (tVIdent id1) (tFIdent id2) $ tExpr $ tAssignOp opassign3 (I.EFieldE (I.EVar id1) id2) expr4
  I.SAssignOpThis id2 opassign3 expr4 s -> tSem s $ return $
    O.SPutField O.VariableThis undefined (tFIdent id2) (tExpr $ tAssignOp opassign3 (I.EFieldE I.EThis id2) expr4) undefined
  I.SPostInc id s -> tStmt $ I.SAssignOp id I.APlus (I.ELitInt 1) s
  I.SPostDec id s -> tStmt $ I.SAssignOp id I.AMinus (I.ELitInt 1) s
  I.SEmpty s -> tSem s $ return O.SEmpty
  I.SAssign id expr s -> tSem s $ return $ O.T_SAssign (tVIdent id) $ tExpr expr
  I.SAssignArr id expr1 expr2 s -> tSem s $  return $ O.T_SAssignArr (tVIdent id) (tExpr expr1) (tExpr expr2)
  I.SAssignFld id1 id2 expr3 s -> tSem s $ return $ O.T_SAssignFld (tVIdent id1) (tFIdent id2) (tExpr expr3)
  I.SAssignThis id2 expr3 s -> tSem s $ return $
    O.SPutField O.VariableThis undefined (tFIdent id2) (tExpr expr3) undefined
  I.SReturn expr s -> tSem s $ return $ O.SReturn (tExpr expr) undefined
  I.SReturnV s -> tSem s $ return O.SReturnV
  I.SIf expr stmt -> return $ O.SIf (tExpr expr) (O.SBlock $ tStmt stmt)
  I.SIfElse expr stmt1 stmt2 -> return $ O.SIfElse (tExpr expr) (O.SBlock $ tStmt stmt1) (O.SBlock $ tStmt stmt2)
  I.SWhile expr stmt -> return $ O.SWhile (tExpr expr) (O.SBlock $ tStmt stmt)
  I.SForeach typ id expr stmt ->  -- SYNTACTIC SUGAR
    let idarr = tempIdent id "arr"
        idlength = tempIdent id "length"
        iditer = tempIdent id "iter"
    in tStmt $ I.SBlock noLbr [
      I.SDeclVar (I.TComposed $ I.TArray typ) [I.Init idarr expr] noSem,
      I.SDeclVar (I.TPrimitive I.TInt) [I.Init idlength (I.EFieldE (I.EVar idarr) (I.Ident "length")),
      I.Init iditer (I.ELitInt 0)] noSem,
      I.SWhile (I.ERel (I.EVar iditer) I.LTH (I.EVar idlength)) (I.SBlock noLbr [
        I.SDeclVar typ [I.Init id (I.EArrayE (I.EVar idarr) (I.EVar iditer))] noSem,
        stmt,
        I.SPostInc iditer noSem] noRbr)] noRbr
  I.SExpr expr s -> tSem s $ return $ O.SExpr $ tExpr expr
  I.SThrow expr s -> tSem s $ return $ O.SThrow $ tExpr expr
  I.STryCatch stmt1 typ2 id3 stmt4 -> return $ O.T_STryCatch (O.SBlock $ tStmt stmt1) (tTComposed typ2) (tVIdent id3) (O.SBlock $ tStmt stmt4)
  where
  tAssignOp :: I.AssignOp -> I.Expr -> I.Expr -> I.Expr
  tAssignOp opassign expr1 expr2 = case opassign of
    I.APlus -> I.EAdd expr1 I.Plus expr2
    I.AMinus -> I.EAdd expr1 I.Minus expr2
    I.ATimes -> I.EMul expr1 I.Times expr2
    I.ADiv -> I.EMul expr1 I.Div expr2
    I.AMod -> I.EMul expr1 I.Mod expr2

tSem :: I.Semicolon -> [O.Stmt] -> [O.Stmt]
tSem (I.Semicolon ((line, _), _)) stmts
  | line >= 0 = [O.SMetaLocation (Err.Line line) stmts]
  | otherwise = stmts

brToLoc :: I.LeftBrace -> I.RightBrace -> Location
brToLoc (I.LeftBrace ((start, _), _)) (I.RightBrace ((end, _), _))
  | start >= 0 && end >= start = Err.Range start end
  | otherwise = Err.Unknown

tBraces :: I.LeftBrace -> I.RightBrace -> [O.Stmt] -> [O.Stmt]
tBraces lbr rbr stmts = let rng = brToLoc lbr rbr in case rng of
  Err.Range _ _ -> [O.SMetaLocation rng stmts]
  Err.Unknown -> stmts

noSem :: I.Semicolon
noSem = I.Semicolon ((-1, -1), ";")
noLbr :: I.LeftBrace
noLbr = I.LeftBrace ((-1, -1), ";")
noRbr :: I.RightBrace
noRbr = I.RightBrace ((-1, -1), ";")

tItem :: I.TypeBasic -> I.Item -> [O.Stmt]
tItem typ x = case x of
  I.NoInit id -> [O.T_SDeclVar (tTBasic typ) (tVIdent id)]
  -- We use temporary variable with lifetime limited to four statements,
  -- which cannot hide any user-defined variable
  I.Init id expr ->  -- SYNTACTIC SUGAR
    let idtmp = tempIdent id "decl"
    in [
      O.T_SDeclVar (tTBasic typ) (tVIdent idtmp),
      O.T_SAssign (tVIdent idtmp) (tExpr expr),
      O.T_SDeclVar (tTBasic typ) (tVIdent id),
      O.T_SAssign (tVIdent id) (O.T_EVar (tVIdent idtmp))]

tExpr :: I.Expr -> O.Expr
tExpr x = case x of
  I.EVar id -> O.T_EVar $ tVIdent id
  I.ELitInt n -> O.ELitInt n
  I.ELitTrue -> O.ELitTrue
  I.ELitFalse -> O.ELitFalse
  I.EString str -> O.ELitString str
  I.ELitChar c -> O.ELitChar c
  I.ENull -> O.ENull
  I.ENullT typ -> O.ENull
  I.EArrayE expr1 expr2 -> O.EArrayLoad (tExpr expr1) (tExpr expr2) undefined
  I.EMethodE expr id exprs -> O.EInvokeVirtual (tExpr expr) undefined (tMIdent id) (map tExpr exprs)
  I.EFieldE expr id -> O.EGetField (tExpr expr) undefined (tFIdent id) undefined
  I.EArrayI ide expr2 ->
    O.EArrayLoad (tExpr $ I.EVar ide) (tExpr expr2) undefined -- GRAMMAR IRREGULARITY
  I.EMethodI ide id exprs ->
    O.EInvokeVirtual (tExpr $ I.EVar ide) undefined (tMIdent id) (map tExpr exprs) -- GRAMMAR IRREGULARITY
  I.EFieldI ide id ->
    O.EGetField (tExpr $ I.EVar ide) undefined (tFIdent id)  undefined -- GRAMMAR IRREGULARITY
  I.EFieldIT id -> O.EGetField (O.ELoad O.VariableThis undefined) undefined (tFIdent id) undefined
  I.EMethodIT id exprs ->
    O.EInvokeVirtual (O.ELoad O.VariableThis undefined) undefined (tMIdent id) (map tExpr exprs)
  I.EApp id exprs -> O.EInvokeStatic undefined (tMIdent id) (map tExpr exprs)
  I.ENewArray typ expr -> O.ENewArr (tTBasic typ) (tExpr expr)
  I.ENewObject typ -> O.ENewObj $ tTComposed typ
  I.ENeg expr -> O.EUnary O.OuNeg (tExpr expr) undefined
  I.ENot expr -> O.EUnary O.OuNot (tExpr expr) undefined
  I.EMul expr1 opbin2 expr3 -> O.EBinary (tMulOp opbin2) (tExpr expr1) (tExpr expr3) undefined
  I.EAdd expr1 opbin2 expr3 -> O.EBinary (tAddOp opbin2) (tExpr expr1) (tExpr expr3) undefined
  I.ERel expr1 opbin2 expr3 -> O.EBinary (tRelOp opbin2) (tExpr expr1) (tExpr expr3) undefined
  I.EAnd expr1 expr3 -> O.EBinary O.ObAnd (tExpr expr1) (tExpr expr3) undefined
  I.EOr expr1 expr3 -> O.EBinary O.ObOr (tExpr expr1) (tExpr expr3) undefined
  I.EThis -> O.ELoad O.VariableThis undefined

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

tTBasic :: I.TypeBasic -> O.TypeBasic
tTBasic x = case x of
  I.TComposed typ -> O.TComposed $ tTComposed typ
  I.TPrimitive typ -> O.TPrimitive $ tTPrimitive typ

tTComposed :: I.TypeComposed -> O.TypeComposed
tTComposed x = case x of
  I.TString -> O.TString
  I.TUser id -> O.TUser (tTIdent id)
  I.TObject -> O.TObject
  I.TArray typ -> O.TArray $ tTBasic typ

tTPrimitive :: I.TypePrimitive -> O.TypePrimitive
tTPrimitive x = case x of
  I.TVoid -> O.TVoid
  I.TInt -> O.TInt
  I.TChar -> O.TChar
  I.TBool -> O.TBool

tVIdent :: I.Ident -> O.VariableName
tVIdent (I.Ident id) = O.VariableName id
tMIdent :: I.Ident -> O.MethodName
tMIdent (I.Ident id) = O.MethodName id
tTIdent :: I.Ident -> O.ClassName
tTIdent (I.Ident id) = O.ClassName id
tFIdent :: I.Ident -> O.FieldName
tFIdent (I.Ident id) = O.FieldName id


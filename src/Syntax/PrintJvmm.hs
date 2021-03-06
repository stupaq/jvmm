{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Syntax.PrintJvmm where

-- pretty-printer generated by the BNF converter

import Data.Char
import Syntax.AbsJvmm


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))


instance Print Semicolon where
  prt _ (Semicolon (_,i)) = doc (showString ( i))


instance Print LeftBrace where
  prt _ (LeftBrace (_,i)) = doc (showString ( i))


instance Print RightBrace where
  prt _ (RightBrace (_,i)) = doc (showString ( i))



instance Print Program where
  prt i e = case e of
   Program definitions -> prPrec i 0 (concatD [prt 0 definitions])


instance Print Definition where
  prt i e = case e of
   DFunction function -> prPrec i 0 (concatD [prt 0 function])
   DClass class' -> prPrec i 0 (concatD [prt 0 class'])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Function where
  prt i e = case e of
   Function typebasic id arguments exceptions leftbrace stmts rightbrace -> prPrec i 0 (concatD [prt 0 typebasic , prt 0 id , doc (showString "(") , prt 0 arguments , doc (showString ")") , prt 0 exceptions , prt 0 leftbrace , prt 0 stmts , prt 0 rightbrace])


instance Print Argument where
  prt i e = case e of
   Argument typebasic id -> prPrec i 0 (concatD [prt 0 typebasic , prt 0 id])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Exceptions where
  prt i e = case e of
   NoExceptions  -> prPrec i 0 (concatD [])
   Exceptions typecomposeds -> prPrec i 0 (concatD [doc (showString "throws") , prt 0 typecomposeds])


instance Print Class where
  prt i e = case e of
   Class id extends leftbrace members rightbrace -> prPrec i 0 (concatD [doc (showString "class") , prt 0 id , prt 0 extends , prt 0 leftbrace , prt 0 members , prt 0 rightbrace])


instance Print Member where
  prt i e = case e of
   FieldsList typebasic fields semicolon -> prPrec i 0 (concatD [prt 0 typebasic , prt 0 fields , prt 0 semicolon])
   Method function -> prPrec i 0 (concatD [prt 0 function])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Field where
  prt i e = case e of
   Field id -> prPrec i 0 (concatD [prt 0 id])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Extends where
  prt i e = case e of
   SuperClass typecomposed -> prPrec i 0 (concatD [doc (showString "extends") , prt 0 typecomposed])
   SuperObject  -> prPrec i 0 (concatD [])


instance Print TypeBasic where
  prt i e = case e of
   TComposed typecomposed -> prPrec i 0 (concatD [prt 0 typecomposed])
   TPrimitive typeprimitive -> prPrec i 0 (concatD [prt 0 typeprimitive])


instance Print TypeComposed where
  prt i e = case e of
   TObject  -> prPrec i 0 (concatD [doc (showString "Object")])
   TUser id -> prPrec i 0 (concatD [prt 0 id])
   TArray typebasic -> prPrec i 0 (concatD [prt 0 typebasic , doc (showString "[]")])
   TString  -> prPrec i 0 (concatD [doc (showString "string")])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print TypePrimitive where
  prt i e = case e of
   TInt  -> prPrec i 0 (concatD [doc (showString "int")])
   TChar  -> prPrec i 0 (concatD [doc (showString "char")])
   TBool  -> prPrec i 0 (concatD [doc (showString "boolean")])
   TVoid  -> prPrec i 0 (concatD [doc (showString "void")])


instance Print Stmt where
  prt i e = case e of
   SThrow expr semicolon -> prPrec i 0 (concatD [doc (showString "throw") , prt 0 expr , prt 0 semicolon])
   STryCatch stmt0 typecomposed id stmt -> prPrec i 0 (concatD [doc (showString "try") , prt 0 stmt0 , doc (showString "catch") , doc (showString "(") , prt 0 typecomposed , prt 0 id , doc (showString ")") , prt 0 stmt])
   SBlock leftbrace stmts rightbrace -> prPrec i 0 (concatD [prt 0 leftbrace , prt 0 stmts , prt 0 rightbrace])
   SEmpty semicolon -> prPrec i 0 (concatD [prt 0 semicolon])
   SDeclVar typebasic items semicolon -> prPrec i 0 (concatD [prt 0 typebasic , prt 0 items , prt 0 semicolon])
   SAssign expr0 expr semicolon -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "=") , prt 0 expr , prt 0 semicolon])
   SAssignOp expr0 assignop expr semicolon -> prPrec i 0 (concatD [prt 0 expr0 , prt 0 assignop , prt 0 expr , prt 0 semicolon])
   SPostInc expr semicolon -> prPrec i 0 (concatD [prt 0 expr , doc (showString "++") , prt 0 semicolon])
   SPostDec expr semicolon -> prPrec i 0 (concatD [prt 0 expr , doc (showString "--") , prt 0 semicolon])
   SReturn expr semicolon -> prPrec i 0 (concatD [doc (showString "return") , prt 0 expr , prt 0 semicolon])
   SReturnV semicolon -> prPrec i 0 (concatD [doc (showString "return") , prt 0 semicolon])
   SIf expr stmt -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 expr , doc (showString ")") , prt 0 stmt])
   SIfElse expr stmt0 stmt -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 expr , doc (showString ")") , prt 0 stmt0 , doc (showString "else") , prt 0 stmt])
   SWhile expr stmt -> prPrec i 0 (concatD [doc (showString "while") , doc (showString "(") , prt 0 expr , doc (showString ")") , prt 0 stmt])
   SForeach typebasic id expr stmt -> prPrec i 0 (concatD [doc (showString "for") , doc (showString "(") , prt 0 typebasic , prt 0 id , doc (showString ":") , prt 0 expr , doc (showString ")") , prt 0 stmt])
   SExpr expr semicolon -> prPrec i 0 (concatD [prt 0 expr , prt 0 semicolon])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Item where
  prt i e = case e of
   NoInit id -> prPrec i 0 (concatD [prt 0 id])
   Init id expr -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 expr])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Expr where
  prt i e = case e of
   EVar id -> prPrec i 7 (concatD [prt 0 id])
   ELitInt n -> prPrec i 7 (concatD [prt 0 n])
   ELitTrue  -> prPrec i 7 (concatD [doc (showString "true")])
   ELitFalse  -> prPrec i 7 (concatD [doc (showString "false")])
   EString str -> prPrec i 7 (concatD [prt 0 str])
   EThis  -> prPrec i 7 (concatD [doc (showString "self")])
   ENull  -> prPrec i 7 (concatD [doc (showString "null")])
   ENullT typecomposed -> prPrec i 7 (concatD [doc (showString "(") , prt 0 typecomposed , doc (showString ")null")])
   ELitChar c -> prPrec i 7 (concatD [prt 0 c])
   EArray expr0 expr -> prPrec i 6 (concatD [prt 6 expr0 , doc (showString "[") , prt 0 expr , doc (showString "]")])
   EField expr id -> prPrec i 6 (concatD [prt 6 expr , doc (showString ".") , prt 0 id])
   EMethod expr id exprs -> prPrec i 6 (concatD [prt 6 expr , doc (showString ".") , prt 0 id , doc (showString "(") , prt 0 exprs , doc (showString ")")])
   ENewObject typecomposed -> prPrec i 5 (concatD [doc (showString "new") , prt 0 typecomposed])
   ENewArray typebasic expr -> prPrec i 5 (concatD [doc (showString "new") , prt 0 typebasic , doc (showString "[") , prt 0 expr , doc (showString "]")])
   EApp id exprs -> prPrec i 5 (concatD [prt 0 id , doc (showString "(") , prt 0 exprs , doc (showString ")")])
   ENeg expr -> prPrec i 5 (concatD [doc (showString "-") , prt 5 expr])
   ENot expr -> prPrec i 5 (concatD [doc (showString "!") , prt 5 expr])
   EMul expr0 mulop expr -> prPrec i 4 (concatD [prt 4 expr0 , prt 0 mulop , prt 5 expr])
   EAdd expr0 addop expr -> prPrec i 3 (concatD [prt 3 expr0 , prt 0 addop , prt 4 expr])
   ERel expr0 relop expr -> prPrec i 2 (concatD [prt 2 expr0 , prt 0 relop , prt 3 expr])
   EAnd expr0 expr -> prPrec i 1 (concatD [prt 1 expr0 , doc (showString "&&") , prt 2 expr])
   EOr expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "||") , prt 1 expr])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print AssignOp where
  prt i e = case e of
   APlus  -> prPrec i 0 (concatD [doc (showString "+=")])
   AMinus  -> prPrec i 0 (concatD [doc (showString "-=")])
   ATimes  -> prPrec i 0 (concatD [doc (showString "*=")])
   ADiv  -> prPrec i 0 (concatD [doc (showString "/=")])
   AMod  -> prPrec i 0 (concatD [doc (showString "%=")])


instance Print AddOp where
  prt i e = case e of
   Plus  -> prPrec i 0 (concatD [doc (showString "+")])
   Minus  -> prPrec i 0 (concatD [doc (showString "-")])


instance Print MulOp where
  prt i e = case e of
   Times  -> prPrec i 0 (concatD [doc (showString "*")])
   Div  -> prPrec i 0 (concatD [doc (showString "/")])
   Mod  -> prPrec i 0 (concatD [doc (showString "%")])


instance Print RelOp where
  prt i e = case e of
   LTH  -> prPrec i 0 (concatD [doc (showString "<")])
   LEQ  -> prPrec i 0 (concatD [doc (showString "<=")])
   GTH  -> prPrec i 0 (concatD [doc (showString ">")])
   GEQ  -> prPrec i 0 (concatD [doc (showString ">=")])
   EQU  -> prPrec i 0 (concatD [doc (showString "==")])
   NEQ  -> prPrec i 0 (concatD [doc (showString "!=")])




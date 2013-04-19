module Semantics.Builtins where
import qualified Data.Map as Map
import Semantics.Trans (UIdent(..))
import Syntax.AbsJvmm (Ident(..), Type(..))

-- TODO this is hackish, fix it when the time comes
entrypoint = Ident "main$0"

-- Types of builtin functions
types = Map.fromList [
  (FIdent "printInt$0", TFunc TVoid [TInt] []),
  (FIdent "readInt$0", TFunc TInt [] []),
  (FIdent "printString$0", TFunc TVoid [] []),
  (FIdent "readString$0", TFunc TString [] []),
  (FIdent "error$0", TFunc TVoid [] [])]

-- Names of builtin functions
names = map (\(FIdent id) -> Ident $ takeWhile (/= '$') id) $ Map.keys types


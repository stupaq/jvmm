module Scope where
import qualified Data.Map as Map
import qualified Syntax.AbsJvmm as AST

-- Intermediate layer between syntax and semantics dependent parts
type Ident = AST.Ident

data VarEntry = VarEntry {}
data FuncEntry = FuncEntry {}
data TypeEntry = AST.Type

data Scope = Scope {
  funcs :: Map Ident FuncEntry,
  vars :: Map Ident VarEntry,
  types :: Map Ident TypeEntry
}

resolveFunc :: Scope -> Ident -> Maybe FuncEntry
resolveFunc s i = lookup i (funcs s)

resolveVar :: Scope -> Ident -> Maybe FuncEntry
resolveVar s i = lookup i (vars s)

resolveType :: Scope -> Ident -> Maybe TypeEntry
resolveType s i = lookup i (types s)

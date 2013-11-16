module Jvmm.Hierarchy where
import Jvmm.Hierarchy.Internal
import Jvmm.Hierarchy.Output

import Control.Monad.Identity
import Data.List as List

import Jvmm.Builtins (builtins)
import qualified Jvmm.Errors as Err
import Jvmm.Errors (rethrow, ErrorInfoT)
import Jvmm.Trans.Output

-- CLASSES DIFFERENCE --
------------------------
-- Prepares class diff, a function which returns full class description (inclluding superclass
-- members) when provided with superclass description.
prepareClassDiff :: Class -> ClassDiff
prepareClassDiff clazz super = do
    guard (classType super == classSuper clazz)
    fields <- fieldsClosure (classFields clazz) (classFields super)
    methods <- methodsClosure (classMethods clazz) (classMethods super)
    return clazz {
      classFields = fields,
      classMethods = methods
    }

-- CLASS HIERARCHY --
---------------------
hierarchy :: CompilationUnit -> ErrorInfoT Identity ClassHierarchy
hierarchy (CompilationUnit allClasses) = case allClasses of
  (TUnknown, objectDiff):classes -> visit objectSuperClass classes objectDiff
  _ -> error $ Err.unusedBranch "no TObject diff"

-- OBJECT CLASS --
------------------
objectClassDiff :: [Method] -> ErrorInfoT Identity ClassDiff
objectClassDiff functions = do
  guard (repeated == []) `rethrow` Err.repeatedDeclaration (head repeated)
  guard (redefined == []) `rethrow` Err.redefinedBuiltin (head redefined)
  return $ prepareClassDiff Class {
      classType = TObject,
      classSuper = TUnknown,
      classFields = [],
      classMethods = builtins ++ functions
    }
  where
    repeated, idents, redefined :: [UIdent]
    repeated = idents \\ (nub idents)  -- This can be done faster but not funnier
    idents = List.map methodIdent functions
    redefined = let builtinIdents = List.map methodIdent builtins
      in builtinIdents `intersect` idents


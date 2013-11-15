module Jvmm.Hierarchy (hierarchy, prepareClassDiff, HierarchyM) where
import Jvmm.Hierarchy.Internal

import Control.Monad
import Control.Monad.Identity

import Semantics.Commons
import qualified Jvmm.Errors as Err
import Jvmm.Errors (ErrorInfoM, ErrorInfoT)
import Jvmm.Trans.Output hiding (HierarchyM)

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


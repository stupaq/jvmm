module Jvmm.Hierarchy where
import Jvmm.Hierarchy.Internal
import Jvmm.Hierarchy.Output

import Control.Monad.Identity
import Data.List as List

import Jvmm.Builtins (builtinFunctions)
import qualified Jvmm.Errors as Err
import Jvmm.Errors (rethrow, ErrorInfoT)
import Jvmm.Trans.Output

-- CLASSES DIFFERENCE --
------------------------
-- Prepares class diff, a function which returns full class description (inclluding superclass
-- members) when provided with superclass description.
prepareClassDiff :: Err.Location -> Class -> ClassDiff
prepareClassDiff loc clazz super = Err.withLocation loc $ do
    guard (classType super == classSuper clazz)
    guard (clashing == []) `rethrow` Err.staticNonStaticConflict (head clashing)
    fields' <- fieldsClosure fields (classFields super)
    methods' <- methodsClosure methods (classMethods super)
    staticMethods' <- methodsClosure staticMethods (classStaticMethods super)
    return clazz {
        classFields = fields'
      , classMethods = methods'
      , classStaticMethods = staticMethods'
      , classLocation = loc
    }
  where
    clashing :: [MethodName]
    clashing = (List.map methodName methods) `intersect` (List.map methodName staticMethods)
    fields :: [Field]
    fields = [ field { fieldOrigin = typ } | field <- classFields clazz]
    methodsWithOrigin :: (Class -> [Method]) -> [Method]
    methodsWithOrigin accessor =
      List.map (\method -> method { methodOrigin = typ }) $ accessor clazz
    methods, staticMethods :: [Method]
    methods = methodsWithOrigin classMethods
    staticMethods = methodsWithOrigin classStaticMethods

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
  return $ prepareClassDiff Err.Unknown Class {
        className = classname0
      , classSuper = TUnknown
      , classFields = []
      , classMethods = []
      , classStaticMethods = builtinFunctions ++ functions
      , classLocation = Err.Unknown
    }
  where
    repeated, idents, redefined :: [MethodName]
    repeated = idents \\ (nub idents)  -- This can be done faster but not funnier
    idents = List.map methodName functions
    redefined = let builtinIdents = List.map methodName builtinFunctions
      in builtinIdents `intersect` idents


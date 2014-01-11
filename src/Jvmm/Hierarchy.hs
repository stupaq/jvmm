module Jvmm.Hierarchy where
import Jvmm.Hierarchy.Internal

import Control.Monad.Identity
import Data.List as List

import qualified Jvmm.Builtins as Builtins
import qualified Jvmm.Errors as Err
import Jvmm.Errors (rethrow, ErrorInfoT)
import Jvmm.Trans.Output

-- HIERARCHY ------------------------------------------------------------------
--  The layer responsible for determining inheritance hierarhyc, propagating
--  inherited fields and methods down the tree and verifying that there is no
--  ambiguity when refering a field or method.
-------------------------------------------------------------------------------

-- Prepares class diff, a function which returns full class description (inclluding superclass
-- members) when provided with superclass description.
prepareClassDiff :: Err.Location -> Class -> ClassDiff
prepareClassDiff loc clazz@(Class { classType = typ }) super = Err.withLocation loc $ do
    guard (classType super == classSuper clazz)
    guard (null clashing) `rethrow` Err.staticNonStaticConflict (head clashing)
    fields' <- fieldsClosure fields (classFields super)
    instanceMethods' <- methodsClosure instanceMethods (classInstanceMethods super)
    staticMethods' <- methodsClosure staticMethods (classStaticMethods super)
    return clazz {
        classFields = fields'
      , classAllMethods = instanceMethods' ++ staticMethods'
      , classLocation = loc
    }
  where
    clashing :: [MethodName]
    clashing = List.map methodName instanceMethods `intersect` List.map methodName staticMethods
    fields :: [Field]
    fields = [ field { fieldOrigin = typ } | field <- classFields clazz]
    methodsWithOrigin :: (Class -> [Method]) -> [Method]
    methodsWithOrigin accessor =
      List.map (\method -> method { methodOrigin = typ }) $ accessor clazz
    instanceMethods, staticMethods :: [Method]
    instanceMethods = methodsWithOrigin classInstanceMethods
    staticMethods = methodsWithOrigin classStaticMethods

hierarchy :: CompilationUnit -> ErrorInfoT Identity ClassHierarchy
hierarchy (CompilationUnit allClasses) = case allClasses of
  (_, objectDiff):classes -> visit objectSuperClass classes objectDiff
  _ -> Err.unreachable "no primary object diff"

-- TObject class, an ancestor for every other class in the hierarchy
objectClassDiff :: [Method] -> ErrorInfoT Identity ClassDiff
objectClassDiff functions = do
  guard (null repeated) `rethrow` Err.repeatedDeclaration (head repeated)
  guard (null redefined) `rethrow` Err.redefinedBuiltin (head redefined)
  return $ prepareClassDiff Err.Unknown Class {
      -- Object is its own supertype
        classType = TObject
      , classSuper = TObject
      , classFields = []
      , classAllMethods = Builtins.libraryMethods ++ functions
      , classLocation = Err.Unknown
    }
  where
    repeated, idents, redefined :: [MethodName]
    repeated = idents \\ nub idents  -- This can be done faster but not funnier
    idents = List.map methodName functions
    redefined = let builtinIdents = List.map methodName Builtins.libraryMethods
      in builtinIdents `intersect` idents


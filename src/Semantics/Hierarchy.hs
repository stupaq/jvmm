module Semantics.Hierarchy (hierarchy, prepareClassDiff, HierarchyM) where

import Prelude hiding (id)
import Control.Monad
import Control.Monad.Identity
import Data.List as List

import Semantics.Commons
import qualified Semantics.Errors as Err
import Semantics.Errors (rethrow, ErrorInfoM, ErrorInfoT)
import Semantics.APTree hiding (HierarchyM)

-- HIERACHY MONAD --
--------------------
type HierarchyM = ErrorInfoT Identity

-- CLASS CLOSURE --
-------------------
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
  where
    fieldsClosure :: [Field] -> [Field] -> HierarchyM [Field]
    fieldsClosure clazz super = do
      let repeated = List.nub [ x | Field _ x <- clazz, Field _ y <- super, x == y ]
      guard (repeated == []) `rethrow` Err.redeclaredInSuper repeated
      return $ super ++ clazz
    methodsClosure :: [Method] -> [Method] -> HierarchyM [Method]
    methodsClosure clazz super = do
      let repeated = List.nub [ x | Method tx x _ _ <- clazz, Method ty y _ _ <- super, x == y, tx /= ty ]
      guard (repeated == []) `rethrow` Err.redeclaredWithDifferentType repeated
      -- This will leave the last occurence of a function with given name
      return $ List.nubBy eqMethodName $ clazz ++ super
      where
        eqMethodName :: Method -> Method -> Bool
        eqMethodName (Method _ x _ _) (Method _ y _ _) = x == y
        eqMethodName _ _ = False

-- CLASS HIERARCHY --
---------------------
objectSuperClass :: Class
objectSuperClass = Class {
  classType = TUnknown,
  classSuper = TUnknown,
  classFields = [],
  classMethods = []
}

hierarchy :: CompilationUnit -> ErrorInfoT Identity ClassHierarchy
hierarchy (CompilationUnit allClasses) = case allClasses of
  (TUnknown, objectDiff):classes -> visit objectSuperClass classes objectDiff
  _ -> error $ Err.unusedBranch "no TObject diff"

visit :: Class -> [(Type, ClassDiff)] -> ClassDiff -> HierarchyM ClassHierarchy
visit super classes diff = do
  current <- diff super
  children <- mapM (visit current classes) [ diff | (typ, diff) <- classes, typ == classType current ]
  return $ Node current children


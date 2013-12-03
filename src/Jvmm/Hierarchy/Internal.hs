module Jvmm.Hierarchy.Internal where
import Jvmm.Hierarchy.Output

import Control.Monad
import Control.Monad.Identity
import qualified Data.List as List

import qualified Jvmm.Errors as Err
import Jvmm.Errors (rethrow, ErrorInfoT)
import Jvmm.Trans.Output

-- HIERACHY MONAD --
--------------------
type HierarchyM = ErrorInfoT Identity

-- CLASS CLOSURE --
-------------------
fieldsClosure :: [Field] -> [Field] -> HierarchyM [Field]
fieldsClosure clazz super = do
  let repeated = List.nub [ x |
        Field { fieldName = x } <- clazz,
        Field { fieldName = y } <- super,
        x == y ]
  guard (repeated == []) `rethrow` Err.redeclaredInSuper repeated
  return $ super ++ clazz

methodsClosure :: [Method] -> [Method] -> HierarchyM [Method]
methodsClosure clazz superOrig = do
  -- Strip down super class' methods implementation
  let super = List.map (\method -> method { methodBody = SInherited }) superOrig
  let repeated = List.nub [ x |
        Method { methodType = tx, methodName = x } <- clazz,
        Method { methodType = ty, methodName = y } <- super,
        x == y, tx /= ty ]
  guard (repeated == []) `rethrow` Err.redeclaredWithDifferentType repeated
  -- This will leave the last occurence of a function with given name
  return $ List.nubBy eqMethodName $ clazz ++ super
  where
    eqMethodName :: Method -> Method -> Bool
    eqMethodName x y = methodName x == methodName y

-- CLASS HIERARCHY --
---------------------
objectSuperClass :: Class
objectSuperClass = Class {
  -- Object is its own supertype
    classType = TObject
  , classSuper = undefined
  , classFields = []
  , classMethods = []
  , classStaticMethods = []
  , classLocation = Err.Unknown
}

visit :: Class -> [(TypeComposed, ClassDiff)] -> ClassDiff -> HierarchyM ClassHierarchy
visit super classes diff = do
  current@Class {} <- diff super
  children <- mapM (visit current classes)
      [ diff | (typ, diff) <- classes, typ == classType current ]
  return $ Node current children


module Jvmm.Hierarchy.Internal where

import Control.Monad
import Control.Monad.Identity

import qualified Data.List as List
import qualified Data.Tree as Tree

import Jvmm.Errors (ErrorInfoT, rethrow)
import qualified Jvmm.Errors as Err
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
  guard (null repeated) `rethrow` Err.redeclaredInSuper repeated
  return $ super ++ clazz

methodsClosure :: [Method] -> [Method] -> HierarchyM [Method]
methodsClosure clazz superOrig = do
  -- Strip down super class' methods implementation
  let super = List.map (\method -> method { methodBody = SInherited }) superOrig
  let repeated = List.nub [ x |
        Method { methodType = tx, methodName = x } <- clazz,
        Method { methodType = ty, methodName = y } <- super,
        x == y, tx /= ty ]
  guard (null repeated) `rethrow` Err.redeclaredWithDifferentType repeated
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
  , classAllMethods = []
  , classLocation = Err.Unknown
}

visit :: Class -> [(TypeComposed, ClassDiff)] -> ClassDiff -> HierarchyM ClassHierarchy
visit super classes diff = do
  current <- diff super
  children <- mapM (visit current classes)
      [ child | (typ, child) <- classes, typ == classType current ]
  return $ Tree.Node current children


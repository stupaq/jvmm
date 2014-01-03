{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Jvmm.LlvmEmitter.Layout where

import Control.Monad.Identity

import qualified Data.Map as Map
import qualified Data.Tree as Tree

import Jvmm.Builtins
import Jvmm.Trans.Output

-- HIERARCHY LAYOUT --
----------------------
data ClassLayout = ClassLayout {
    layoutFieldOffsets  :: Map.Map FieldName Int
  , layoutMethodOffsets :: Map.Map MethodName Int
  , layoutInit          :: [Stmt]
} deriving (Show)

classlayout0 :: ClassLayout
classlayout0 = ClassLayout Map.empty Map.empty []

type HierarchyLayout = Map.Map TypeComposed ClassLayout

-- CLASSES LAYOUT --
--------------------
layoutHierarchy :: ClassHierarchy -> Identity HierarchyLayout
layoutHierarchy = return . layout classlayout0
  where
    layout :: ClassLayout -> ClassHierarchy -> HierarchyLayout
    layout parent Tree.Node { Tree.rootLabel = clazz, Tree.subForest = children } =
      let Class { classType = typ } = clazz
      in let current = build parent clazz
        in foldl Map.union (Map.singleton typ current) $ map (layout current) children
    build :: ClassLayout -> Class -> ClassLayout
    build (ClassLayout fields methods ctor) clazz =
      let cFields = classFields clazz
      in ClassLayout {
          layoutFieldOffsets = foldl insertField fields cFields
        , layoutMethodOffsets = foldl insertMethod methods $ classInstanceMethods clazz
        , layoutInit = ctor ++ map initField cFields
      }
      where
        cTyp = classType clazz
        insertField x Field { fieldName = name } = Map.insert name (Map.size x) x
        insertMethod x Method { methodName = name } =
          if Map.member name x then x else Map.insert name (Map.size x) x
        initField Field { fieldName = name, fieldType = typ } = SAssign {
              sassignTarget = LField (LVariable VariableThis $ TComposed cTyp) cTyp name typ
            , sassignSource = defaultValue typ
          }


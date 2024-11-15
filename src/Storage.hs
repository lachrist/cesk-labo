{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Storage where

import Control.Arrow (second)
import Domain (BuiltinName, Domain (Builtin, Closure, Pair, Primitive))
import Environment (Environment)
import Expression (Expression, Variable)
import Primitive (Primitive)
import Serial (Serial (StructureNode), Serializable (serialize))
import Store (Address, Store, find, load, push, save)

class Storage x v where
  new :: Store x -> Domain v -> (Store x, v)
  get :: Store x -> v -> Domain v
  set :: Store x -> v -> Domain v -> Either String (Store x)

data StorageSystem
  = NoStorage
  | CompleteStorage
  | ReuseCompleteStorage
  | HybridStorage
  deriving (Eq, Show)

----------------
-- No Storage --
----------------

newtype VoidItem
  = VoidItem ()
  deriving (Eq, Show)

newtype InlineValue
  = InlineValue (Domain InlineValue)
  deriving (Eq, Show)

instance Storage VoidItem InlineValue where
  new mem raw = (mem, InlineValue raw)
  get _ (InlineValue raw) = raw
  set _ _ _ = Left "mutation not supported in inline storage"

instance Serializable InlineValue where
  serialize (InlineValue raw) = serialize raw

instance Serializable VoidItem where
  serialize _ = StructureNode "void" []

----------------------
-- Complete Storage --
----------------------

newtype ComprehensiveItem
  = ComprehensiveItem (Domain ReferenceValue)
  deriving (Eq, Show)

newtype ReferenceValue
  = ReferenceValue Address
  deriving (Eq, Show)

instance Storage ComprehensiveItem ReferenceValue where
  new mem raw = second ReferenceValue (push mem (ComprehensiveItem raw))
  get mem (ReferenceValue ref) = let (ComprehensiveItem raw) = load mem ref in raw
  set mem (ReferenceValue ref) raw = Right $ save mem (ref, ComprehensiveItem raw)

instance Serializable ReferenceValue where
  serialize (ReferenceValue ref) = serialize ref

instance Serializable ComprehensiveItem where
  serialize (ComprehensiveItem raw) = serialize raw

-------------------------------------
-- Complete Storage with Interning --
-------------------------------------

newtype ReuseComprehensiveItem
  = ReuseComprehensiveItem (Domain ReuseReferenceValue)
  deriving (Eq, Show)

newtype ReuseReferenceValue
  = ReuseReferenceValue Address
  deriving (Eq, Show)

matchPrimitive :: Primitive -> ReuseComprehensiveItem -> Bool
matchPrimitive prm1 (ReuseComprehensiveItem (Primitive prm2)) = prm1 == prm2
matchPrimitive _ _ = False

instance Storage ReuseComprehensiveItem ReuseReferenceValue where
  new mem raw@(Primitive _) =
    case find mem (ReuseComprehensiveItem raw) of
      Just ref -> (mem, ReuseReferenceValue ref)
      Nothing -> second ReuseReferenceValue (push mem (ReuseComprehensiveItem raw))
  new mem raw = second ReuseReferenceValue (push mem (ReuseComprehensiveItem raw))
  get mem (ReuseReferenceValue ref) = let (ReuseComprehensiveItem raw) = load mem ref in raw
  set mem (ReuseReferenceValue ref) raw = Right $ save mem (ref, ReuseComprehensiveItem raw)

instance Serializable ReuseReferenceValue where
  serialize (ReuseReferenceValue ref) = serialize ref

instance Serializable ReuseComprehensiveItem where
  serialize (ReuseComprehensiveItem raw) = serialize raw

------------
-- Hybrid --
------------

data HybridItem
  = PairHybridItem HybridValue HybridValue
  | BuiltinHybridItem BuiltinName
  | ClosureHybridItem (Environment HybridValue) [Variable] Expression
  deriving (Eq, Show)

data HybridValue
  = InlineHybridValue Primitive
  | ReferenceHybridValue Address
  deriving (Eq, Show)

toDomain :: HybridItem -> Domain HybridValue
toDomain (PairHybridItem fst snd) = Pair fst snd
toDomain (BuiltinHybridItem tag) = Builtin tag
toDomain (ClosureHybridItem env head body) = Closure env head body

toItem :: Domain HybridValue -> HybridItem
toItem (Primitive _) = error "cannot convert primitive to item"
toItem (Pair fst snd) = PairHybridItem fst snd
toItem (Builtin tag) = BuiltinHybridItem tag
toItem (Closure env head body) = ClosureHybridItem env head body

instance Storage HybridItem HybridValue where
  new mem (Primitive prm) = (mem, InlineHybridValue prm)
  new mem raw = second ReferenceHybridValue (push mem (toItem raw))
  get _ (InlineHybridValue prm) = Primitive prm
  get mem (ReferenceHybridValue ref) = toDomain $ load mem ref
  set _ (InlineHybridValue _) _ = Left "cannot mutate a primitive value"
  set _ _ (Primitive _) = Left "cannot mutate to primitive value"
  set mem (ReferenceHybridValue ref) raw = Right $ save mem (ref, toItem raw)

instance Serializable HybridValue where
  serialize (InlineHybridValue prm) = serialize prm
  serialize (ReferenceHybridValue ref) = serialize ref

instance Serializable HybridItem where
  serialize = serialize . toDomain

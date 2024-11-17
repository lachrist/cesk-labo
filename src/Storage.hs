{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Storage where

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
  | ComprehensiveStorage
  | ReuseComprehensiveStorage
  | HybridStorage
  deriving (Eq, Show)

----------------
-- No Storage --
----------------

newtype VoidItem
  = VoidItem ()
  deriving (Eq, Show)

newtype ImmediateValue
  = ImmediateValue (Domain ImmediateValue)
  deriving (Eq, Show)

instance Storage VoidItem ImmediateValue where
  new ::
    Store VoidItem ->
    Domain ImmediateValue ->
    (Store VoidItem, ImmediateValue)
  new store datum = (store, ImmediateValue datum)
  get ::
    Store VoidItem ->
    ImmediateValue ->
    Domain ImmediateValue
  get _ (ImmediateValue datum) = datum
  set ::
    Store VoidItem ->
    ImmediateValue ->
    Domain ImmediateValue ->
    Either String (Store VoidItem)
  set _ _ _ = Left "mutation not supported in inline storage"

instance Serializable ImmediateValue where
  serialize (ImmediateValue datum) = serialize datum

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
  new ::
    Store ComprehensiveItem ->
    Domain ReferenceValue ->
    (Store ComprehensiveItem, ReferenceValue)
  new store datum =
    let (store', address) = push store (ComprehensiveItem datum)
     in (store', ReferenceValue address)
  get ::
    Store ComprehensiveItem ->
    ReferenceValue ->
    Domain ReferenceValue
  get store (ReferenceValue address) =
    let ComprehensiveItem datum = load store address
     in datum
  set ::
    Store ComprehensiveItem ->
    ReferenceValue ->
    Domain ReferenceValue ->
    Either String (Store ComprehensiveItem)
  set store (ReferenceValue address) datum =
    Right $ save store (address, ComprehensiveItem datum)

instance Serializable ReferenceValue where
  serialize (ReferenceValue address) = serialize address

instance Serializable ComprehensiveItem where
  serialize (ComprehensiveItem datum) = serialize datum

-------------------------------------
-- Complete Storage with Interning --
-------------------------------------

newtype ReuseComprehensiveItem
  = ReuseComprehensiveItem (Domain ReuseReferenceValue)
  deriving (Eq, Show)

newtype ReuseReferenceValue
  = ReuseReferenceValue Address
  deriving (Eq, Show)

instance Storage ReuseComprehensiveItem ReuseReferenceValue where
  new ::
    Store ReuseComprehensiveItem ->
    Domain ReuseReferenceValue ->
    (Store ReuseComprehensiveItem, ReuseReferenceValue)
  new store datum =
    let reuse = case datum of
          (Primitive _) -> find store (ReuseComprehensiveItem datum)
          _ -> Nothing
     in case reuse of
          Just address -> (store, ReuseReferenceValue address)
          Nothing ->
            let (store', address) = push store (ReuseComprehensiveItem datum)
             in (store', ReuseReferenceValue address)
  get ::
    Store ReuseComprehensiveItem ->
    ReuseReferenceValue ->
    Domain ReuseReferenceValue
  get store (ReuseReferenceValue address) =
    let (ReuseComprehensiveItem datum) = load store address
     in datum
  set ::
    Store ReuseComprehensiveItem ->
    ReuseReferenceValue ->
    Domain ReuseReferenceValue ->
    Either String (Store ReuseComprehensiveItem)
  set store (ReuseReferenceValue address) datum =
    Right $ save store (address, ReuseComprehensiveItem datum)

instance Serializable ReuseReferenceValue where
  serialize (ReuseReferenceValue address) = serialize address

instance Serializable ReuseComprehensiveItem where
  serialize (ReuseComprehensiveItem datum) = serialize datum

------------
-- Hybrid --
------------

data HybridItem
  = PairHybridItem HybridValue HybridValue
  | BuiltinHybridItem BuiltinName
  | ClosureHybridItem (Environment HybridValue) [Variable] Expression
  deriving (Eq, Show)

data HybridValue
  = ImmediateHybridValue Primitive
  | ReferenceHybridValue Address
  deriving (Eq, Show)

toDomain :: HybridItem -> Domain HybridValue
toDomain (PairHybridItem first second) = Pair first second
toDomain (BuiltinHybridItem name) = Builtin name
toDomain (ClosureHybridItem env head body) = Closure env head body

toItem :: Domain HybridValue -> HybridItem
toItem (Primitive _) = error "cannot convert primitive to item"
toItem (Pair first second) = PairHybridItem first second
toItem (Builtin name) = BuiltinHybridItem name
toItem (Closure environment head body) = ClosureHybridItem environment head body

instance Storage HybridItem HybridValue where
  new ::
    Store HybridItem ->
    Domain HybridValue ->
    (Store HybridItem, HybridValue)
  new store (Primitive primitive) = (store, ImmediateHybridValue primitive)
  new store datum =
    let (store', address) = push store (toItem datum)
     in (store', ReferenceHybridValue address)
  get ::
    Store HybridItem ->
    HybridValue ->
    Domain HybridValue
  get _ (ImmediateHybridValue primitive) = Primitive primitive
  get store (ReferenceHybridValue address) = toDomain $ load store address
  set ::
    Store HybridItem ->
    HybridValue ->
    Domain HybridValue ->
    Either String (Store HybridItem)
  set _ (ImmediateHybridValue _) _ = Left "cannot mutate an immediate value"
  set _ _ (Primitive _) = Left "reference value cannot be a primitive"
  set store (ReferenceHybridValue address) datum =
    Right $ save store (address, toItem datum)

instance Serializable HybridValue where
  serialize (ImmediateHybridValue primitive) = serialize primitive
  serialize (ReferenceHybridValue address) = serialize address

instance Serializable HybridItem where
  serialize = serialize . toDomain

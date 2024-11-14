{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Storage where

import Data (BuiltinName, Data (Builtin, Closure, Cons, Primitive))
import Data.Sequence (Seq, findIndexL, index, update, (|>))
import Environment (Environment)
import Expression (Expression, Variable)
import Primitive (Primitive)

class Storage s v | s -> v where
  new :: s -> Data v -> (s, v)
  get :: s -> v -> Data v
  set :: s -> v -> Data v -> Either String s

----------
-- Void --
----------

newtype VoidStore = VoidStore ()

newtype InlineValue = InlineValue (Data InlineValue) deriving (Eq, Show)

instance Storage VoidStore InlineValue where
  new mem arg = (mem, InlineValue arg)
  get _ (InlineValue arg) = arg
  set _ _ _ = Left "mutation not supported in inline storage"

initialVoidStore :: VoidStore
initialVoidStore = VoidStore ()

-------------
-- Storage --
-------------

newtype FullStore = FullStore (Seq (Data ReferenceValue))

newtype ReferenceValue = ReferenceValue Int deriving (Eq, Show)

instance Storage FullStore ReferenceValue where
  new (FullStore seq) arg = (FullStore $ seq |> arg, ReferenceValue $ length seq)
  get (FullStore seq) (ReferenceValue ref) = index seq ref
  set (FullStore seq) (ReferenceValue ref) arg = Right $ FullStore $ update ref arg seq

initialFullStore :: FullStore
initialFullStore = FullStore mempty

---------------
-- ReuseFull --
---------------

newtype ReuseFullStore = ReuseFullStore (Seq (Data ReuseReferenceValue))

newtype ReuseReferenceValue = ReuseReferenceValue Int deriving (Eq, Show)

matchPrimitive :: Primitive -> Data ReuseReferenceValue -> Bool
matchPrimitive prm1 (Primitive prm2) = prm1 == prm2
matchPrimitive _ _ = False

instance Storage ReuseFullStore ReuseReferenceValue where
  new (ReuseFullStore seq) arg@(Primitive prm) =
    case findIndexL (matchPrimitive prm) seq of
      Just ref -> (ReuseFullStore seq, ReuseReferenceValue ref)
      Nothing -> (ReuseFullStore $ seq |> arg, ReuseReferenceValue $ length seq)
  new (ReuseFullStore seq) arg = (ReuseFullStore $ seq |> arg, ReuseReferenceValue $ length seq)
  get (ReuseFullStore seq) (ReuseReferenceValue ref) = index seq ref
  set (ReuseFullStore seq) (ReuseReferenceValue ref) arg = Right $ ReuseFullStore $ update ref arg seq

initialReuseFullStore :: ReuseFullStore
initialReuseFullStore = ReuseFullStore mempty

------------
-- Hybrid --
------------

newtype HybridStore = HybridStore (Seq Item)

data Item
  = ConsItem HybridValue HybridValue
  | BuiltinItem BuiltinName
  | ClosureItem (Environment HybridValue) [Variable] Expression

data HybridValue
  = InlineHybridValue Primitive
  | ReferenceHybridValue Int
  deriving (Eq, Show)

toHybridValue :: Item -> Data HybridValue
toHybridValue (ConsItem car cdr) = Cons car cdr
toHybridValue (BuiltinItem name) = Builtin name
toHybridValue (ClosureItem env vars expr) = Closure env vars expr

toItem :: Data HybridValue -> Item
toItem (Primitive _) = error "cannot convert primitive to item"
toItem (Cons car cdr) = ConsItem car cdr
toItem (Builtin name) = BuiltinItem name
toItem (Closure env vars expr) = ClosureItem env vars expr

instance Storage HybridStore HybridValue where
  new mem (Primitive prm) = (mem, InlineHybridValue prm)
  new (HybridStore seq) arg = (HybridStore $ seq |> toItem arg, ReferenceHybridValue $ length seq)
  get _ (InlineHybridValue prm) = Primitive prm
  get (HybridStore seq) (ReferenceHybridValue ref) = toHybridValue $ index seq ref
  set _ (InlineHybridValue _) _ = Left "cannot mutate a primitive value"
  set _ _ (Primitive _) = Left "cannot mutate to primitive value"
  set (HybridStore seq) (ReferenceHybridValue ref) arg =
    Right $ HybridStore $ update ref (toItem arg) seq

initialHybridStore :: HybridStore
initialHybridStore = HybridStore mempty

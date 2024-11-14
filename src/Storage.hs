{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Storage where

import Data (BuiltinName, Data (Builtin, Closure, Cons, Primitive))
import Data.Sequence (Seq, findIndexL, index, update, (|>))
import Environment (Environment)
import Expression (Expression, Variable)
import Formatable (Formatable (format), Tree (Atom, List))
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

instance Formatable InlineValue where
  format (InlineValue raw) = format raw

instance Storage VoidStore InlineValue where
  new mem raw = (mem, InlineValue raw)
  get _ (InlineValue raw) = raw
  set _ _ _ = Left "mutation not supported in inline storage"

instance Formatable VoidStore where
  format (VoidStore _) = List []

initialVoidStore :: VoidStore
initialVoidStore = VoidStore ()

-------------
-- Storage --
-------------

newtype FullStore = FullStore (Seq (Data ReferenceValue))

newtype ReferenceValue = ReferenceValue Int deriving (Eq, Show)

instance Storage FullStore ReferenceValue where
  new (FullStore seq) raw = (FullStore $ seq |> raw, ReferenceValue $ length seq)
  get (FullStore seq) (ReferenceValue ref) = index seq ref
  set (FullStore seq) (ReferenceValue ref) raw = Right $ FullStore $ update ref raw seq

instance Formatable ReferenceValue where
  format (ReferenceValue ref) = Atom $ "@" ++ show ref

instance Formatable FullStore where
  format (FullStore seq) = format seq

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
  new (ReuseFullStore seq) raw@(Primitive prm) =
    case findIndexL (matchPrimitive prm) seq of
      Just ref -> (ReuseFullStore seq, ReuseReferenceValue ref)
      Nothing -> (ReuseFullStore $ seq |> raw, ReuseReferenceValue $ length seq)
  new (ReuseFullStore seq) raw = (ReuseFullStore $ seq |> raw, ReuseReferenceValue $ length seq)
  get (ReuseFullStore seq) (ReuseReferenceValue ref) = index seq ref
  set (ReuseFullStore seq) (ReuseReferenceValue ref) raw = Right $ ReuseFullStore $ update ref raw seq

instance Formatable ReuseReferenceValue where
  format (ReuseReferenceValue ref) = Atom $ "@" ++ show ref

instance Formatable ReuseFullStore where
  format (ReuseFullStore seq) = format seq

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

toData :: Item -> Data HybridValue
toData (ConsItem car cdr) = Cons car cdr
toData (BuiltinItem name) = Builtin name
toData (ClosureItem env vars expr) = Closure env vars expr

toItem :: Data HybridValue -> Item
toItem (Primitive _) = error "cannot convert primitive to item"
toItem (Cons car cdr) = ConsItem car cdr
toItem (Builtin name) = BuiltinItem name
toItem (Closure env vars expr) = ClosureItem env vars expr

instance Storage HybridStore HybridValue where
  new mem (Primitive prm) = (mem, InlineHybridValue prm)
  new (HybridStore seq) raw = (HybridStore $ seq |> toItem raw, ReferenceHybridValue $ length seq)
  get _ (InlineHybridValue prm) = Primitive prm
  get (HybridStore seq) (ReferenceHybridValue ref) = toData $ index seq ref
  set _ (InlineHybridValue _) _ = Left "cannot mutate a primitive value"
  set _ _ (Primitive _) = Left "cannot mutate to primitive value"
  set (HybridStore seq) (ReferenceHybridValue ref) raw =
    Right $ HybridStore $ update ref (toItem raw) seq

instance Formatable HybridValue where
  format (InlineHybridValue prm) = format prm
  format (ReferenceHybridValue ref) = Atom $ "@" ++ show ref

instance Formatable Item where
  format = format . toData

instance Formatable HybridStore where
  format (HybridStore seq) = format seq

initialHybridStore :: HybridStore
initialHybridStore = HybridStore mempty

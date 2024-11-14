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

-------------------------------
-- Storage1 >> All Reference --
-------------------------------

newtype Store1 = Store1 (Seq Item1)

empty1 :: Store1
empty1 = Store1 mempty

type Item1 = Data Value1

newtype Value1 = Reference1 Int deriving (Eq, Show)

instance Storage Store1 Value1 where
  new :: Store1 -> Data Value1 -> (Store1, Value1)
  new (Store1 seq) arg = (Store1 $ seq |> arg, Reference1 $ length seq)
  get :: Store1 -> Value1 -> Data Value1
  get (Store1 seq) (Reference1 ref) = index seq ref
  set :: Store1 -> Value1 -> Data Value1 -> Either String Store1
  set (Store1 seq) (Reference1 ref) arg = Right $ Store1 $ update ref arg seq

----------------------------
-- Storage2 >> All Inline --
----------------------------

newtype Store2 = Store2 ()

empty2 :: Store2
empty2 = Store2 ()

newtype Value2 = Inline2 (Data Value2) deriving (Eq, Show)

instance Storage Store2 Value2 where
  new :: Store2 -> Data Value2 -> (Store2, Value2)
  new mem arg = (mem, Inline2 arg)
  get :: Store2 -> Value2 -> Data Value2
  get _ (Inline2 arg) = arg
  set :: Store2 -> Value2 -> Data Value2 -> Either String Store2
  set _ _ _ = Left "mutation not supported in inline storage"

-----------------------
-- Storage3 >> Mixed --
-----------------------

newtype Store3 = Store3 (Seq Item3)

empty3 :: Store3
empty3 = Store3 mempty

data Item3
  = Cons3 Value3 Value3
  | Builtin3 BuiltinName
  | Closure3 (Environment Value3) [Variable] Expression

data Value3
  = Inline3 Primitive
  | Reference3 Int
  deriving (Eq, Show)

toValue3 :: Item3 -> Data Value3
toValue3 (Cons3 car cdr) = Cons car cdr
toValue3 (Builtin3 name) = Builtin name
toValue3 (Closure3 env vars expr) = Closure env vars expr

toItem3 :: Data Value3 -> Item3
toItem3 (Primitive _) = error "cannot convert primitive to item"
toItem3 (Cons car cdr) = Cons3 car cdr
toItem3 (Builtin name) = Builtin3 name
toItem3 (Closure env vars expr) = Closure3 env vars expr

instance Storage Store3 Value3 where
  new :: Store3 -> Data Value3 -> (Store3, Value3)
  new mem (Primitive prm) = (mem, Inline3 prm)
  new (Store3 seq) arg = (Store3 $ seq |> toItem3 arg, Reference3 $ length seq)
  get :: Store3 -> Value3 -> Data Value3
  get _ (Inline3 prm) = Primitive prm
  get (Store3 seq) (Reference3 ref) = toValue3 $ index seq ref
  set :: Store3 -> Value3 -> Data Value3 -> Either String Store3
  set _ (Inline3 _) _ = Left "cannot mutate a primitive value"
  set _ _ (Primitive _) = Left "cannot mutate to primitive value"
  set (Store3 seq) (Reference3 ref) arg =
    Right $ Store3 $ update ref (toItem3 arg) seq

---------------------------
-- Storage4 >> Interning --
---------------------------

newtype Store4 = Store4 (Seq Item4)

empty4 :: Store4
empty4 = Store4 mempty

type Item4 = Data Value4

newtype Value4 = Reference4 Int deriving (Eq, Show)

matchPrimitive4 :: Primitive -> Item4 -> Bool
matchPrimitive4 prm (Primitive prm') = prm == prm'
matchPrimitive4 _ _ = False

instance Storage Store4 Value4 where
  new :: Store4 -> Data Value4 -> (Store4, Value4)
  new (Store4 seq) arg@(Primitive prm) =
    case findIndexL (matchPrimitive4 prm) seq of
      Just ref -> (Store4 seq, Reference4 ref)
      Nothing -> (Store4 $ seq |> arg, Reference4 $ length seq)
  new (Store4 seq) arg = (Store4 $ seq |> arg, Reference4 $ length seq)
  get :: Store4 -> Value4 -> Data Value4
  get (Store4 seq) (Reference4 ref) = index seq ref
  set :: Store4 -> Value4 -> Data Value4 -> Either String Store4
  set (Store4 seq) (Reference4 ref) arg = Right $ Store4 $ update ref arg seq

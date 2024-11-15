{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Store (Store, Address, initial, load, save, push, find) where

import Data.Foldable (toList)
import Data.Sequence (Seq, findIndexL, index, update, (|>))
import Serial (Serial (MappingNode, SymbolLeaf), Serializable (serialize))

newtype Address = Address Int deriving (Eq, Show)

newtype Store v = Store (Seq v) deriving (Eq, Show)

initial :: Store x
initial = Store mempty

load :: Store x -> Address -> x
load (Store memory) (Address key) = index memory key

save :: Store x -> (Address, x) -> Store x
save (Store memory) (Address key, item) = Store $ update key item memory

push :: Store x -> x -> (Store x, Address)
push (Store memory) item = (Store $ memory |> item, Address $ length memory)

find :: (Eq x) => Store x -> x -> Maybe Address
find (Store memory) item = Address <$> findIndexL (== item) memory

indexes :: [Int]
indexes = [0 ..]

instance Serializable Address where
  serialize (Address index) = SymbolLeaf $ "&" ++ show index

serializeEntry :: (Serializable x) => (Int, x) -> (String, Serial)
serializeEntry (index, item) = ("&" ++ show index, serialize item)

instance (Serializable x) => Serializable (Store x) where
  serialize (Store memory) = MappingNode $ zipWith (curry serializeEntry) indexes $ toList memory

{-# LANGUAGE FunctionalDependencies #-}

module System where

import Data (Data)
import Data.List ()
import Data.Map (empty)
import Format ()
import Primitive ()
import State ()
import Store (Store (..))

class (Eq v) => System v x | x -> v where
  dataToValue :: Store x -> Data v -> (Store x, v)
  valueToData :: Store x -> v -> Data v
  mutate :: Store x -> v -> Data v -> Either String (Store x)
  initialStore :: Store x
  initialStore = Store empty
  inspect :: Store x -> v -> String

-- ---------------------------------------
-- -- System1: Data Passed-By-Reference --
-- ---------------------------------------

-- -- BEGIN LISTING System1 --
-- data Value1 = Address1 Address

-- instance Eq Value1 where
--   (Address1 α) == (Address1 α') = α == α'

-- data Element1 = Data1 (Data Value1)

-- fresh :: Store e -> Address
-- fresh σ = maybe 0 ((+ 1) . fst) (Data.Map.lookupMax σ)

-- instance System Value1 Element1 where
--   dataToValue σ δ = (σ', Address1 α)
--     where
--       α = fresh σ
--       σ' = Data.Map.insert α (Data1 δ) σ
--   valueToData σ (Address1 α) = δ
--     where
--       (Data1 δ) = σ Data.Map.! α
--   mutate σ (Address1 α) δ = Right σ'
--     where
--       σ' = Data.Map.insert α (Data1 δ) σ

--   -- END LISTING --
--   inspect σ (Address1 α) = "&" ++ show α ++ "[" ++ showData δ ++ "]"
--     where
--       (Data1 δ) = σ Data.Map.! α

-- instance ShowIndent Value1 where
--   showIndent _ (Address1 α) = "&" ++ show α

-- instance ShowIndent Element1 where
--   showIndent ι (Data1 δ) = showIndent ι δ

-- -------------------------------------------------------------------------------
-- -- System2: Atomic Data Passed-By-Value & Compound Data Passed-By-Reference  --
-- -------------------------------------------------------------------------------

-- -- BEGIN LISTING System2 --
-- data Value2
--   = Atomic2 Atomic
--   | Address2 Address

-- instance Eq Value2 where
--   (Atomic2 θ) == (Atomic2 θ') = θ == θ'
--   (Address2 α) == (Address2 α') = α == α'

-- data Element2 = Compound2 (Compound Value2)

-- instance System Value2 Element2 where
--   dataToValue σ (Atomic θ) = (σ, Atomic2 θ)
--   dataToValue σ (Compound π) = (σ', Address2 α)
--     where
--       α = fresh σ
--       σ' = Data.Map.insert α (Compound2 π) σ
--   valueToData σ (Atomic2 θ) = Atomic θ
--   valueToData σ (Address2 α) = Compound π
--     where
--       (Compound2 π) = σ Data.Map.! α
--   mutate σ (Address2 α) (Compound π) = Right σ'
--     where
--       σ' = Data.Map.insert α (Compound2 π) σ
--   mutate _ _ _ = Left "Cannot mutate atomic data"

--   -- END LISTING --
--   inspect :: Store Element2 -> Value2 -> String
--   inspect σ (Atomic2 θ) = showAtomic θ
--   inspect σ (Address2 α) = "&" ++ show α ++ "[" ++ showCompound π ++ "]"
--     where
--       (Compound2 π) = σ Data.Map.! α

-- instance ShowIndent Value2 where
--   showIndent ι (Atomic2 δ) = showIndent ι δ
--   showIndent ι (Address2 α) = "&" ++ show α

-- instance ShowIndent Element2 where
--   showIndent ι (Compound2 δ) = showIndent ι δ

-- ----------------------------------------------------
-- -- System3: Data Passed-By-Reference & Interning  --
-- ----------------------------------------------------

-- -- BEGIN LISTING System3 --
-- data Value3 = Address3 Address

-- instance Eq Value3 where
--   (Address3 α) == (Address3 α') = α == α'

-- data Element3 = Data3 (Data Value3)

-- interns :: [Atomic]
-- interns =
--   [ Literal Null,
--     Literal (Boolean True),
--     Literal (Boolean False),
--     Literal (Number 0),
--     Literal (Number $ 0 / 0)
--   ]

-- instance System Value3 Element3 where
--   dataToValue :: Store Element3 -> Data Value3 -> (Store Element3, Value3)
--   dataToValue σ δ = maybe (σ', Address3 α) ifJust μ
--     where
--       μ = (toAtomic δ) >>= (`Data.List.elemIndex` interns)
--       ifJust ν = (σ, Address3 $ fromIntegral ν)
--       α = fresh σ
--       σ' = Data.Map.insert α (Data3 δ) σ
--   valueToData σ (Address3 α) = δ
--     where
--       (Data3 δ) = σ Data.Map.! α
--   mutate σ (Address3 α) δ
--     | fromIntegral α < length interns = Left "Mutations disabled"
--     | otherwise = Right $ Data.Map.insert α (Data3 δ) σ
--   initialStore = Data.Map.fromAscList $ zip [0 ..] (map (Data3 . Atomic) interns)

--   -- END LISTING --
--   inspect σ (Address3 α) = "&" ++ show α ++ "[" ++ showData δ ++ "]"
--     where
--       (Data3 δ) = σ Data.Map.! α

-- instance ShowIndent Value3 where
--   showIndent _ (Address3 α) = "&" ++ show α

-- instance ShowIndent Element3 where
--   showIndent ι (Data3 δ) = showIndent ι δ

-- -----------------------------------
-- -- System4: Data Passed-By-Value --
-- -----------------------------------

-- data Value4 = Data4 (Data Value4)

-- instance Eq Value4 where
--   (Data4 δ) == (Data4 δ') = error "data are not equalable"

-- data Element4 = Void

-- instance System Value4 Element4 where
--   dataToValue σ δ = (σ, Data4 δ)
--   valueToData _ (Data4 δ) = δ
--   mutate _ _ _ = Left "System \"inline\" does not support mutation"
--   inspect _ (Data4 δ) = showData δ

-- instance ShowIndent Value4 where
--   showIndent ι (Data4 δ) = showIndent ι δ

-- instance ShowIndent Element4 where
--   showIndent _ (Void) = error "Element4 should never be instantiated"

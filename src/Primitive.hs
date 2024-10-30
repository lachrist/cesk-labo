{-# LANGUAGE InstanceSigs #-}

module Primitive where

import Format

data Primitive
  = Null
  | Boolean Bool
  | Number Float
  | String String
  deriving (Eq, Show)

instance Format Primitive where
  format :: Int -> Primitive -> String
  format _ Null = "#n"
  format _ (Boolean True) = "#t"
  format _ (Boolean False) = "#f"
  format _ (Number float) = show float
  format _ (String characters) = show characters
